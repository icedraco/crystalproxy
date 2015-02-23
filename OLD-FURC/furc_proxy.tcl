##[1.1.0]##########[http://icerealm.loktuslumina.com]##
# furc_proxy.tcl # Furcadian Proxy Script             #
##[icedragon@quickfox.org]#############################

namespace eval ::furc {
	# Variables #
	variable server  {64.191.51.88 6000}
	variable port    {8082}
	variable version {1.1.0}

	# Functions #
	proc cleanup        {} {} ;# Dispose of all FURC objects
	proc version        {} {} ;# Returns the version number

	# Namespaces #
	namespace eval convert {} ;# Conversion functions
	namespace eval user    {} ;# User database management
	namespace eval fd      {} ;# File Descriptor management
	namespace eval hand    {} ;# Primary data-handling
	namespace eval hook    {} ;# Event hook handling
}

### PRIMARY NAMESPACE #################################
proc ::furc::cleanup {} {
	# Purging DBs and closing sockets #
	foreach fd [::furc::fd::list] {
		[set fd]::del
	}

	# Dropping the listening socket #
	listen [set ::furc::port] off

	# Destroying namespaces #
	namespace delete ::furc::convert
	namespace delete ::furc::user
	namespace delete ::furc::fd
	namespace delete ::furc::hand
	namespace delete ::furc::hook

	namespace delete ::furc
}

proc ::furc::version {} {
	set [namespace current]::version
}


### CONVERSION NAMESPACE ##############################
namespace eval ::furc::convert {
	# Functions #
	proc atoi     {} {} ;# Character to integer ~| Character conversion
	proc itoa     {} {} ;# Integer to character _|
	proc stoi     {} {} ;# String to number     ~|
	proc itos     {} {} ;# Number to string     _| FURC Number conversion
	proc dir      {} {} ;# Turn coordinates into a direction (1/3/5/7/9)
	proc dir2sign {} {} ;# Turn a direction into one of the custom arrows
}


proc ::furc::convert::atoi {char} {
	scan $char "%c" c
	return $c
}

proc ::furc::convert::itoa {int} {
	format "%c" $int
}

proc ::furc::convert::stoi {string} {
	scan $string "%c%c" c(0) c(1)

	if {($c(0) < 32) || ($c(0) > 127)} { return -1 }
	if {($c(1) < 32) || ($c(1) > 127)} { return -1 }

	expr (($c(0) - 32) * 95) + ($c(1) - 32)
}

proc ::furc::convert::itos {int} {
	if {$int <= 0} {
		set int 0
	}
	if {$int >= 9025} {
		set int 9025
	}

	format {%c%c} [expr ($int / 95) + 32] [expr ($int % 95) + 32]
}

proc ::furc::convert::dir {src dst} {
	if {$src == $dst} {
		return 5 ;# No movement
	}

	set sx   [lindex $src  0] ;# Source X
	set sy   [lindex $src  1] ;# Source Y
	set dx   [lindex $dst  0] ;# Destination X
	set dy   [lindex $dst  1] ;# Destination Y
	set flag [expr   $sy % 2] ;# 0->1 / 1->0 movement flag

	if {($sy < $dy) && ((!$flag && ($sx == $dx)) || ($flag && ($sx >  $dx)))} { return 1 } ;# _/
	if {($sy < $dy) && ((!$flag && ($sx <  $dx)) || ($flag && ($sx == $dx)))} { return 3 } ;# \_
	if {($sy > $dy) && ((!$flag && ($sx <  $dx)) || ($flag && ($sx == $dx)))} { return 9 } ;# /~
	if {($sy > $dy) && ((!$flag && ($sx == $dx)) || ($flag && ($sx >  $dx)))} { return 7 } ;# ~\

	return 0 ;# Strange direction!
}

proc ::furc::convert::dir2sign {dir} {
	switch $dir {
		{5} {
			return "**"
		}
		{7} {
			return "~\\"
		}
		{9} {
			return "/~"
		}
		{3} {
			return "\\_"
		}
		{1} {
			return "_/"
		}
	}

	return "?!"
}


### USER DATABASE NAMESPACE ###########################
namespace eval ::furc::user {
	# Functions #
	proc add    {} {} ;# Create a database
	proc del    {} {} ;# Purge a database
	proc list   {} {} ;# List existing users
	proc exists {} {} ;# User existence verification
}


proc ::furc::user::add {} {
	for {set i 1} {[exists [namespace current]::$i]} {incr i} {}

	namespace eval ::furc::user::$i {
		# Variables #
		variable name     {(Unknown)} ;# Username
		variable colormap {(Unknown)} ;# Color map
		variable desc     {(Unknown)} ;# Description
		variable location {(Unknown)} ;# Map name
		variable camera   {0 0}       ;# Camera coordinates
		variable frame    {0}         ;# Current frame
		variable logon    {0}         ;# Logon timestamp

		variable fd       {}          ;# FD pointer

		# Functions #
		proc del {} {
			::furc::user::del [namespace current]
		}
		proc set {var value} {
			::set [namespace current]::$var $value
		}
		proc get {var} {
			::set [namespace current]::$var
		}
		proc unset {var} {
			::unset [namespace current]::$var
		}
		proc exists {} {
			return 1 ;# Duh
		}
	}

	return ::furc::user::$i
}

proc ::furc::user::del {user} {
	if {![exists $user]} { return 0 }

	namespace delete $user
}

proc ::furc::user::list {} {
	set children {}

	foreach child [namespace children] {
		if {[string is digit [namespace tail $child]]} {
			lappend children $child
		}
	}

	return $children
}

proc ::furc::user::exists {user} {
	namespace exists $user
}


### SOCKET DATABASE NAMESPACE #########################
namespace eval ::furc::fd {
	# Functions #
	proc add       {} {} ;# Add a descriptor
	proc del       {} {} ;# Remove a descriptor
	proc list      {} {} ;# List file descriptors
	proc find      {} {} ;# Find a descriptor
	proc exists    {} {} ;# Verifies descriptor existence
	proc broadcast {} {} ;# Broadcast to all clients/servers
}


proc ::furc::fd::add {} {
	for {set i 1} {[exists [namespace current]::$i]} {incr i} {}

	namespace eval ::furc::fd::$i {
		# Variables #
		variable server {-1}                ;# Server file descriptor
		variable client {-1}                ;# Client file descriptor
		variable user   [::furc::user::add] ;# User pointer
		variable state  {VOID}              ;# Socket state
		variable hfuncs {}                  ;# Handler functions
		variable addr   {}                  ;# Source address

		# Functions #
		proc puts {destination data} {
			if {[string toupper [lindex $destination 0]] == "C"} {
				# Client #
				putdcc [get client] $data
			} elseif {[string toupper [lindex $destination 0]] == "S"} {
				# Server #
				putdcc [get server] $data
			} else {
				return ;# What the hell do you want?
			}
		}
		proc del {} {
			::furc::fd::del [namespace current]
		}
		proc set {var value} {
			::set [namespace current]::$var $value
		}
		proc get {var} {
			::set [namespace current]::$var
		}
		proc unset {var} {
			::unset [namespace current]::$var
		}
		proc hfunc:add {func} {
			lappend [namespace current]::hfuncs $func
		}
		proc hfunc:del {func} {
			::set func_list {}

			foreach function [get hfuncs] {
				if {$function != $func} {
					lappend func_list $function
				}
			}

			set hfuncs $func_list
		}
		proc exists {} {
			return 1
		}
	}

	return [[::furc::fd::[set i]::get user]::set fd ::furc::fd::[set i]]
}

proc ::furc::fd::del {fd} {
	catch {
		killdcc [[set fd]::get client]
		killdcc [[set fd]::get server]
	}

	[[set fd]::get user]::del

	namespace delete $fd
}

proc ::furc::fd::list {} {
	set children {}

	foreach child [namespace children] {
		if {[string is digit [namespace tail $child]]} {
			lappend children $child
		}
	}

	return $children
}

proc ::furc::fd::find {idx} {
	foreach fd [list] {
		if {([[set fd]::get client] == $idx) || ([[set fd]::get server] == $idx)} {
			return $fd
		}
	}

	return
}

proc ::furc::fd::exists {fd} {
	namespace exists $fd
}

proc ::furc::fd::broadcast {destination data} {
	foreach fd [list] {
		catch {
			[set fd]::puts $destination $data
		}
	}
}


### DATA HANDLER NAMESPACE ############################
namespace eval ::furc::hand {
	# Functions #
	proc listen {} {} ;# Handle incoming connections
	proc data   {} {} ;# Handle existing connections
	proc client {} {} ;# Client->Server data sub-handler
	proc server {} {} ;# Server->Client data sub-handler
}

proc ::furc::hand::listen {idx} {
	# Checking for old/buggy records #
	set old_idx [::furc::fd::find $idx]
	if {$old_idx != {}} {
		putlog "\[FURC\] Dropping an old/buggy FD entry ($old_idx)..."
		[set old_idx]::del
	}
	unset old_idx

	# Creating & Initializing a new record #
	set fd [::furc::fd::add]

	[set fd]::set client  $idx
	[set fd]::set state  {PRE-AUTH}

	foreach dccinfo [dcclist] {
		if {[lindex $dccinfo 0] == $idx} {
			[set fd]::set addr [lindex [split [lindex $dccinfo 2] "@"] 1]
			break
		}
	}

	putlog "\[FURC\] Client connected from [[set fd]::get addr]."

	control $idx ::furc::hand::data
}

proc ::furc::hand::data {idx data} {
	set fd [::furc::fd::find $idx]

	# Checking for information presense #
	if {$fd == {}} {
		catch { killdcc $idx }
		return 1
	}

	# Verifying disconnection #
	if {$data == {}} {
		if {[[set fd]::get client] == $idx} { ;# DISC
			set cause "Client"
		} else {
			set cause "Server"
		}

		# <func> <fd>
		foreach hook [::furc::hook::add $cause DISC] {
			if {![catch {[lindex $hook 1] $fd} ret]} {
				if {$ret != 0} {
					break
				}
			} else {
				putlog "\[FURC\] WARNING: Faulty hook ($hook) -- $ret"
			}
		}

		set name [[[set fd]::get user]::get name]
		putlog [format "\[FURC\] %s disconnected (Name: %s)(Address: %s)" $cause $name [[set fd]::get addr]]
		[set fd]::del
		::furc::fd::broadcast C "(#BL $name has disconnected!"

		return 1
	}

	# Data handling #
	switch [[set fd]::get state] {
		{VOID} {
			# How the heck did we get here with VOID?..
			[set fd]::del
			return 1
		}
		{PRE-AUTH} {
			# Probably gives us a destination... the question is - who gives a damn about it?
			[set fd]::set state {AUTH}

			if {![catch {connect [lindex [set ::furc::server] 0] [lindex [set ::furc::server] 1]} sidx]} {
				control $sidx ::furc::hand::data
				[set fd]::set server $sidx
			} else {
				[set fd]::del
			}
			return 0
		}
		{AUTH} {
			if {[[set fd]::get client] == $idx} {
				# Authentication stage.. here we log in (duh!) - CLIENT
				if {([lindex [split $data] 0] == "connect") && ([llength $data] >= 3)} {
					[[set fd]::get user]::set name [lindex $data 1]
				} elseif {([lindex [split $data] 0] == "color") && ([llength [split $data]] > 1)} {
					[[set fd]::get user]::set colormap [string range $data 6 end]
				} elseif {([lindex [split $data] 0] == "desc") && ([llength [split $data]] > 1)} {
					[[set fd]::get user]::set desc [string range $data 5 end]
				}
			} else {
				# Server-side authentication stage... here we get information (doh!) - SERVER
				if {[string range [split $data] 0 4] == {~}} {
					putlog [format "\[FURC\] %s has connected (Colormap: %s)" [[[set fd]::get user]::get name] [[[set fd]::get user]::get colormap]]
					[set fd]::puts C "(#BK Connected through CrystalWolf Proxy $::furc::version!"
					::furc::fd::broadcast C "(#BJ [[[set fd]::get user]::get name] has connected!"

					[set fd]::puts C "(#BK Currently connected users:"
					foreach usr [::furc::user::list] {
						[set fd]::puts C "(#BK # [[set usr]::get name]"
					}
					[set fd]::puts C "(#BK End of user list. Have fun!"

					[set fd]::set state {LINK}
					if {![catch {[set fd]::get warn}]} {
						[set fd]::puts C "(\[CrystalWolf\] WARNING: The communication protocol in use may not be supported!"
						[set fd]::unset warn
					}
				} elseif {[string index [split $data] 0] == {V}} {
					if {$data != {V0013}} {
						[set fd]::set warn {y}
					}
				}
			}

			# Transmitting #
			if {[[set fd]::get client] == $idx} {
				::furc::hand::client $fd $idx $data
			} else {
				::furc::hand::server $fd $idx $data
			}
			return 0
		}
		{LINK} {
			if {([lindex [split $data] 0] == "color") && ([llength [split $data]] > 1) && ([[[set fd]::get user]::get colormap] != {(Unknown)})} {
				[[set fd]::get user]::set colormap [string range [split $data] 6 end]
			} elseif {([lindex [split $data] 0] == "desc") && ([llength $data] > 1)} {
				[[set fd]::get user]::set desc [string range $data 5 end]
			}
			# Normal data transfer
			if {[[set fd]::get client] == $idx} {
				::furc::hand::client $fd $idx $data
			} else {
				::furc::hand::server $fd $idx $data
			}
			return 0
		}
	}
}

proc ::furc::hand::client {fd idx data} {
	# Pre-Handling #
	foreach function [[set fd]::get hfuncs] {
		if {![catch {$function $fd $idx $data} ret]} {
			if {$ret != {0}} { return $ret }
		} else {
			putlog "\[FURC\] Warning: Pre-handler couldn't execute `$function` successfully for FD `$fd` -- $ret"
		}
	}

	# Processing RAW hook #
	# <func> <fd> <data>
	foreach hook [::furc::hook::add C RAW] {
		if {![catch {[lindex $hook 1] $fd $data} ret]} {
			if {$ret != 0} {
				return 0
			}
		} else {
			putlog "\[FURC\] WARNING: Faulty hook ($hook) -- $ret"
		}
	}

	# Processing Hooks #
	if {[regexp {(:|-|")} [string index $data 0]]} { ;# ACT / SAY / SHOUT "
		set message [string range $data 1 end]

		if {[string index $data 0] == {:}} {
			set h "ACT"
		} elseif {[string index $data 0] == {-}} {
			set h "SHOUT"
		} elseif {[string index $data 0] == "\""} { ;# Stupid editor bleeds "
			set h "SAY"
		} else {
			set h "NULL"
		}

		# <func> <fd> <data>
		foreach hook [::furc::hook::add C $h] {
			if {![catch {[lindex $hook 1] $fd $message} ret]} {
				if {$ret != 0} {
					return 0
				}
			} else {
				putlog "\[FURC\] WARNING: Faulty hook ($hook) -- $ret"
			}
		}
	} elseif {[lindex [split $data] 0] == {gomap}} { ;# GOMAP
		set mapname [string range $data 6 end]

		# <func> <fd> <mapname>
		foreach hook [::furc::hook::add C GOMAP] {
			if {![catch {[lindex $hook 1] $fd $mapname} ret]} {
				if {$ret != 0} {
					return 0
				}
			} else {
				putlog "\[FURC\] WARNING: Faulty hook ($hook) -- $ret"
			}
		}
	} elseif {[lindex [split $data] 0] == {m}} { ;# MOVE
		set dir [lindex [split $data] 1]]

		# <func> <fd> <dir>
		foreach hook [::furc::hook::add C MOVE] {
			if {![catch {[lindex $hook 1] $fd $dir} ret]} {
				if {$ret != 0} {
					return 0
				}
			} else {
				putlog "\[FURC\] WARNING: Faulty hook ($hook) -- $ret"
			}
		}
	} elseif {[string index $data 0] == "\""} { ;# SAY (stupid bleeding editor! ")
		set message [string range $data 1 end]

		# <func> <fd> <data>
		foreach hook [::furc::hook::add C SAY] {
			if {![catch {[lindex $hook 1] $fd $message} ret]} {
				if {$ret != 0} {
					return 0
				}
			} else {
				putlog "\[FURC\] WARNING: Faulty hook ($hook) -- $ret"
			}
		}
	} elseif {[lindex [split $data] 0] == {wh}} { ;# WHISPER
		set space1 [string first { } $data]
		set space2 [expr $space1 + 1 + [string first { } [string range $data [expr $space1 + 1] end]]]

		set dest    [string range $data [expr $space1 + 1] [expr $space2 - 1]]
		set message [string range $data [expr $space2 + 1] end]

		# <func> <fd> <dest> <data>
		foreach hook [::furc::hook::add C WHISPER] {
			if {![catch {[lindex $hook 1] $fd $dest $message} ret]} {
				if {$ret != 0} {
					return 0
				}
			} else {
				putlog "\[FURC\] WARNING: Faulty hook ($hook) -- $ret"
			}
		}
	} elseif {[lindex [split $data] 0] == {quit}} { ;# QUIT
		# <func> <fd>
		foreach hook [::furc::hook::add C QUIT] {
			if {![catch {[lindex $hook 1] $fd} ret]} {
				if {$ret != 0} {
					return 0
				}
			} else {
				putlog "\[FURC\] WARNING: Faulty hook ($hook) -- $ret"
			}
		}
	}

	[set fd]::puts S $data
	return 0
}

proc ::furc::hand::server {fd idx data} {
	# Pre-Handling #
	foreach function [[set fd]::get hfuncs] {
		if {![catch {$function $fd $idx $data} ret]} {
			if {$ret != {0}} { return $ret }
		} else {
			putlog "\[FURC\] Warning: Pre-handler couldn't execute `$function` successfully for FD `$fd` -- $ret"
		}
	}

	# Processing RAW hook #
	# <func> <fd> <data>
	foreach hook [::furc::hook::add S RAW] {
		if {![catch {[lindex $hook 1] $fd $data} ret]} {
			if {$ret != 0} {
				return 0
			}
		} else {
			putlog "\[FURC\] WARNING: Faulty hook ($hook) -- $ret"
			::furc::hook::del S $hook
		}
	}

	# Processing Hooks #
	if {[string index $data 0] == {/}} { ;# MOVE
		set cmap  [string range $data 1 10]
		set frame [::furc::convert::stoi [string range $data 15 16]]
		set dx    [::furc::convert::stoi [string range $data 11 12]]
		set dy    [::furc::convert::stoi [string range $data 13 14]]
		set sx    [::furc::convert::stoi [string range $data 17 18]]
		set sy    [::furc::convert::stoi [string range $data 19 20]]

		set dir   [::furc::convert::dir     "$sx $sy" "$dx $dy"]
		set sign  [::furc::convert::dir2sign $dir]

if {$dir == 5} { return 0 } ;# Annoying!

		set user  [[set fd]::get user]

		# Updating frame record #
		if {[list $dx $dy] == [[set user]::get camera]} {
			[set user]::set frame $frame
		}

		putlog [format "\[FURC\] %s : Movement from %s (%d) \[%d/%d\] -> \[%d/%d\] : \[%d\]\[%s\]" [[[set fd]::get user]::get name] $cmap $frame $sx $sy $dx $dy $dir $sign]

		# <func> <fd> <colormap> <frame> <src_coord> <dst_coord> <direction>
		foreach hook [::furc::hook::add S MOVE] {
			if {![catch {[lindex $hook 1] $fd $cmap $frame [list $sx $sy] [list $dx $dy] $dir} ret]} {
				if {$ret != 0} {
					return 0
				}
			} else {
				putlog "\[FURC\] WARNING: Faulty hook ($hook) -- $ret"
			}
		}
	} elseif {[string range $data 0 1] == {]%}} { ;# NOTIFY
		set online [string index $data 2]
		set name   [string range $data 3 end]

		# <func> <fd> <name> <state>
		foreach hook [::furc::hook::add S NOTIFY] {
			if {![catch {[lindex $hook 1] $fd $name $online} ret]} {
				if {$ret != 0} {
					return 0
				}
			} else {
				putlog "\[FURC\] WARNING: Faulty hook ($hook) -- $ret"
			}
		}
	} elseif {[string index $data 0] == {(}} { ;# MSG
		set message [string range $data 1 end]

		# <func> <fd> <message>
		foreach hook [::furc::hook::add S MSG] {
			if {![catch {[lindex $hook 1] $fd $message} ret]} {
				if {$ret != 0} {
					return 0
				}
			} else {
				putlog "\[FURC\] WARNING: Faulty hook ($hook) -- $ret"
			}
		}

		putlog [format "\[FURC\] %s : %s" [[[set fd]::get user]::get name] [string range $data 1 end]]
	} elseif {[string index $data 0] == {@}} { ;# CAMERA
		set dx   [::furc::convert::stoi [string range $data 1 2]]
		set dy   [::furc::convert::stoi [string range $data 3 4]]

		if {[string length $data] >= 9} {
			set sx [::furc::convert::stoi [string range $data 5 6]]
			set sy [::furc::convert::stoi [string range $data 7 8]]
		} else {
			set sx $dx
			set sy $dy
		}

		[[set fd]::get user]::set camera [::list $dx $dy]

		set dir  [::furc::convert::dir "$sx $sy" "$dx $dy"]

		# <func> <fd> <src_coord> <dst_coord> <direction>
		foreach hook [::furc::hook::add S CAMERA] {
			if {![catch {[lindex $hook 1] $fd [list $sx $sy] [list $dx $dy] $dir} ret]} {
				if {$ret != 0} {
					return 0
				}
			} else {
				putlog "\[FURC\] WARNING: Faulty hook ($hook) -- $ret"
			}
		}
	} elseif {[string range $data 0 1] == {]f}} { ;# LOOK
		set colormap [string range $data 2  14]
		set name     [string range $data 15 end]

putlog "\[LOOK\] [[[set fd]::get user]::get name] : $name - $colormap"

		# <func> <fd> <colormap> <name>
		foreach hook [::furc::hook::add S LOOK] {
			if {![catch {[lindex $hook 1] $fd $colormap $name} ret]} {
				if {$ret != 0} {
					return 0
				}
			} else {
				putlog "\[FURC\] WARNING: Faulty hook ($hook) -- $ret"
			}
		}
	} elseif {[string index $data 0] == ";"} { ;# MAP
		set mapfile [string range $data 1 end]

		[[set fd]::get user]::set location $mapfile

		# <func> <fd> <mapfile>
		foreach hook [::furc::hook::add S MAP] {
			if {![catch {[lindex $hook 1] $fd $mapfile} ret]} {
				if {$ret != 0} {
					return 0
				}
			} else {
				putlog "\[FURC\] WARNING: Faulty hook ($hook) -- $ret"
			}
		}
	} elseif {[string index $data 0] == {<}} { ;#SPAWN
		set colormap [string range $data 1  10]
		set x        [::furc::convert::stoi [string range $data 11 12]]
		set y        [::furc::convert::stoi [string range $data 13 14]]

		if {[set frame [string range $data 15 16]] != {}} {
			set frame [::furc::convert::stoi $frame]
		} else {
			set frame 0
		}

putlog "\[SPWN\] [[[set fd]::get user]::get name] : $colormap \[$x,$y\] - ($frame)"

		set user [[set fd]::get user]

		# Updating frame record #
		if {[list $x $y] == [[set user]::get camera]} {
			[set user]::set frame $frame
		}

		# <func> <fd> <colormap> <frame> <coord>
		foreach hook [::furc::hook::add S SPAWN] {
			if {![catch {[lindex $hook 1] $fd $colormap $frame [list $x $y]} ret]} {
				if {$ret != 0} {
					return 0
				}
			} else {
				putlog "\[FURC\] WARNING: Faulty hook ($hook) -- $ret"
			}
		}
	}


	[set fd]::puts C $data
	return 0
}


### EVENT HOOK MANAGEMENT #############################
namespace eval ::furc::hook {
	# Variables #
	variable client_hooks ;# List of hooks applied (client)
	variable server_hooks ;# List of hooks applied (server)

	# Functions #
	proc add  {} {} ;# Add a hook
	proc del  {} {} ;# Delete a hook
	proc find {} {} ;# Find a hook
	proc list {} {} ;# Lists existing hooks
}

proc ::furc::hook::add {side hook {function {}}} {
	# Sanity checks - side #
	if {![regexp {(S|C)} [string toupper [string index $side 0]]]} {
		return
	}

	# Checking for emptiness #
	if {![info exists [namespace current]::server_hooks]} {
		set [namespace current]::server_hooks {}
	}
	if {![info exists [namespace current]::client_hooks]} {
		set [namespace current]::client_hooks {}
	}

	# Sanity checks - hook #
	if {[string toupper [string index $side 0]] == {S}} {
		if {![regexp {^(CAMERA|DISC|LOOK|MAP|MOVE|MSG|NOTIFY|RAW|SPAWN)$} [string toupper $hook]]} {
			# CAMERA  - Camera motion detection
			# DISC    - Disconnection detection
			# LOOK    - Look response detection (no desc)
			# MAP     - Map switch detection
			# MOVE    - Motion detection
			# MSG     - System message/normal chat/e.t.c.
			# NOTIFY  - Notify (online/offline) announcement
			# RAW     - Raw information stream
			# SPAWN   - Spawn (aka: <) detection
			return
		}
	} else {
		if {![regexp {^(ACT|DISC|GOMAP|MOVE|RAW|SAY|SHOUT|WHISPER|QUIT)$} [string toupper $hook]]} {
			# ACT     - Action detection
			# DISC    - Disconnection detection (client side)
			# GOMAP   - Map switch request detection
			# MOVE    - Movement request detection
			# RAW     - Raw information stream
			# SAY     - Normal chat detection
			# SHOUT   - Shout detection
			# WHISPER - Whisper attempt detection
			# QUIT    - Disconnection request detection
			return
		}
	}

	# Hook listing #
	if {$function == {}} {
		set buffer {}
		foreach entry [list $side] {
			if {[string toupper [lindex $entry 0]] == [string toupper $hook]} {
				lappend buffer $entry
			}
		}
		return $buffer
	}

	# Hook addition (finally!) #
	if {[string toupper [string index $side 0]] == {S}} {
		set var {server_hooks}
	} else {
		set var {client_hooks}
	}

	if {[set oldhook [find $side $function]] != {}} {
		del $side $oldhook
	}
	set newhook [::list [string toupper $hook] $function]

	lappend [namespace current]::$var $newhook
	return $newhook
}

proc ::furc::hook::del {side func} {
	if {[string toupper [string index $side 0]] == {S}} {
		set var {server_hooks}
	} elseif {[string toupper [string index $side 0]] == {C}} {
		set var {client_hooks}
	} else {
		return
	}

	# Is it a function or a hook?
	if {[llength $func] > 1} {
		set flag 1
	} else {
		set flag 0
	}

	set newlist {}
	foreach entry [list $side] {
		if {!$flag && ([lindex $entry 1] != $func)} {
			lappend newlist $entry
		} elseif {$flag && (([lindex $entry 1] != [lindex $func 1]) || ([string toupper [lindex $entry 0]] != [string toupper [lindex $func 0]]))} {
			lappend newlist $entry
		}
	}

	set [namespace current]::$var $newlist
}

proc ::furc::hook::find {side func} {
	foreach entry [list $side] {
		if {[lindex $entry 1] == $func} {
			return $entry
		}
	}

	return
}

proc ::furc::hook::list {side} {
	if {[string toupper [string index $side 0]] == {S}} {
		set var {server_hooks}
	} elseif {[string toupper [string index $side 0]] == {C}} {
		set var {client_hooks}
	} else {
		return
	}

	# Checking for emptiness #
	if {![info exists [namespace current]::server_hooks]} {
		set [namespace current]::server_hooks {}
	}
	if {![info exists [namespace current]::client_hooks]} {
		set [namespace current]::client_hooks {}
	}

	set [namespace current]::$var
}


### INITIALIZATION SEQUENCE ###########################
listen [set ::furc::port] script ::furc::hand::listen
### END OF SCRIPT #####################################

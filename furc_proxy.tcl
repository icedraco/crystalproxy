##[2.1.0]#######################[icedragon@quickfox.org]##
# furc_proxy.tcl # Furcadian Proxy Script # CrystalProxy #
##########################################################
# This script is an eggdrop-based Furcadian proxy core   #
# designed for multi-user data relay and additional      #
# features added externally through its module support.  #
##########################################################
# Tested on eggdrop 1.6.15 with TCL 8.4! TCL 8.3 may     #
# have problems interpreting several functions that are  #
# essential for the script's work!                       #
##########################################################

namespace eval ::furc {
	# Variables #
#	variable server   {quickfox.org 6000}
	variable server   {64.191.51.88 6000}
#	variable server   {66.28.224.193 6000}
	variable port     {9000}
	variable version  {2.1.0}
	variable moddir   {/home/icedragon/crystalwolf/CWScripts/FURC}
	variable modverb  {1}

	# Functions #
	proc cleanup {} {}        ;# Dispose of all FURC objects
	proc version {} {}        ;# Returns the version number

	# SubModules #
	namespace eval convert {} ;# Conversion functions
	namespace eval fd      {} ;# File Descriptor management
	namespace eval hand    {} ;# Central data handlers
	namespace eval hook    {} ;# Even hook management
}


###--# cleanup #--###
proc ::furc::cleanup {} {
	# Dropping the listening socket #
	listen $::furc::port off

	# Sub-module pre-termination hook #
	::furc::hook::term

	# Closing sockets and purging FD data #
	foreach fd [::furc::fd::list] {
		[set fd]::del
	}

	# Destroying sub-modules #
	namespace delete ::furc::convert
	namespace delete ::furc::fd
	namespace delete ::furc::hand
	namespace delete ::furc::hook

	namespace delete ::furc
}


###--# version #--###
proc ::furc::version {} {
	set ::furc::version
}


### CONVERSION SUBMODULE ## [convert] ####################
namespace eval ::furc::convert {
	# Functions #
	proc stoi {} {} ;# base64  -> integer
	proc itos {} {} ;# integer -> base64
	proc dir  {} {} ;# integer -> direction number
}


###--# stoi #--###
proc ::furc::convert::stoi {string} {
	# ASCII -> INT #
	scan $string {%c%c} c(0) c(1)

	# Number sanity check #
	if {($c(0) < 32) || ($c(0) > 127)} { return -1 }
	if {($c(1) < 32) || ($c(1) > 127)} { return -1 }

	# Calculation #
	expr (($c(0) - 32) * 95) + ($c(1) - 32)
}


###--# itos #--###
proc ::furc::convert::itos {int} {
	# Fixing out-ranged numbers if necessary #
	if {$int <= 0}    { set int 0 }
	if {$int >= 9025} { set int 9025 }

	# Calculation #
	format {%c%c} [expr ($int / 95) + 32] [expr ($int % 95) + 32]
}


###--# dir #--###
proc ::furc::convert::dir {src dst} {
	if {$src == $dst} {
		return 5 ;# No movement
	}

	scan $src {%s %s} sx sy  ;# Source coordinates
	scan $dst {%s %s} dx dy  ;# Destination coordinates
	set  flag [expr $sy % 2] ;# X axis flag

	if {($sy < $dy) && ((!$flag && ($sx == $dx)) || ($flag && ($sx >  $dx)))} { return 1 } ;# SW [_/]
	if {($sy < $dy) && ((!$flag && ($sx <  $dx)) || ($flag && ($sx == $dx)))} { return 3 } ;# SE [\_]
	if {($sy > $dy) && ((!$flag && ($sx <  $dx)) || ($flag && ($sx == $dx)))} { return 9 } ;# NE [/~]
	if {($sy > $dy) && ((!$flag && ($sx == $dx)) || ($flag && ($sx >  $dx)))} { return 7 } ;# NW [~\]

	return 0 ;# Unidentified direction
}


### FD MANAGEMENT SUBMODULE ## [fd] ######################
namespace eval ::furc::fd {
	# Functions #
	proc add       {} {} ;# Add a descriptor
	proc del       {} {} ;# Remove a descriptor
	proc list      {} {} ;# List file descriptors
	proc find      {} {} ;# Find a descriptor
	proc exists    {} {} ;# Verifies descriptor existence
	proc broadcast {} {} ;# Broadcast to all clients/servers
}


###--# add #--###
proc ::furc::fd::add {} {
	# Getting an unused socket number #
	for {set i 1} {[exists [namespace current]::$i]} {incr i} {}

	# Creating a namespace #
	namespace eval ::furc::fd::$i {
		# Variables #
		variable server {-1}   ;# Server FD
		variable client {-1}   ;# Client FD
		variable state  {VOID} ;# Socket state
		variable addr   {}     ;# Source address

		###--# puts #--###
		proc puts {dest data} {
			# Verifying direction #
			switch [string toupper [lindex $dest 0]] {
				{C}     {
					# Sending to all the clients assigned to us #
					foreach f [::furc::fd::list] {
						if {([[::set f]::get server] == [get server]) && ([[::set f]::get client] > 0)} {
							putdcc [[::set f]::get client] $data
						}
					}
				}
				{S}     {
					# Sending to all the servers assigned to us #
					foreach f [::furc::fd::list] {
						if {([[::set f]::get client] == [get client]) && ([[::set f]::get server] > 0)} {
							putdcc [[::set f]::get server] $data
						}
					}
				}
				default { return }
			}
		}
		###--# del #--###
		proc del {} {
			::furc::fd::del [namespace current]
		}
		###--# set #--###
		proc set {var value} {
			::set [namespace current]::$var $value
		}
		###--# get #--###
		proc get {var} {
			::set [namespace current]::$var
		}
		###--# unset #--###
		proc unset {var} {
			::unset [namespace current]::$var
		}
		###--# exists #--###
		proc exists {} {
			return 1
		}
	}

	# Returning the new FD entry #
	return ::furc::fd::$i
}


###--# del #--###
proc ::furc::fd::del {fd} {
	# Disposing of CPDB entry #
	if {![catch {::cpdb::version}]} {
		catch {
			::cpdb::[namespace tail $fd]::del
		}
	}

	# Disposing of the namespace #
	namespace delete $fd
}


###--# list #--###
proc ::furc::fd::list {} {
	# Generating the list #
	set children {}

	foreach child [namespace children] {
		if {[string is digit [namespace tail $child]]} {
			lappend children $child
		}
	}

	# Returning #
	return $children
}


###--# find #--###
proc ::furc::fd::find {idx} {
	# Searching #
	foreach fd [list] {
		if {([[set fd]::get client] == $idx) || ([[set fd]::get server] == $idx)} {
			return $fd
		}
	}

	return ;# Not found
}


###--# exists #--###
proc ::furc::fd::exists {fd} {
	namespace exists $fd ;# TCL8.3 doesn't have it!
}


###--# broadcast #--###
proc ::furc::fd::broadcast {dest data {except {}}} {
	foreach fd [list] {
		if {[[set fd]::get state] == {LINK}} {
			if {($except == {}) || (($except != {}) && ($fd != $except))} {
				[set fd]::puts $dest $data
			}
		}
	}
}


### CENTRAL DATA HANDLING SUBMODULE ## [hand] ############
namespace eval ::furc::hand {
	# Functions #
	proc listen {} {} ;# Incoming connection handler
	proc data   {} {} ;# Central handler for existing connections
	proc client {} {} ;# Client->Server sub-handler
	proc server {} {} ;# Server->Client sub-handler
}


###--# listen #--###
proc ::furc::hand::listen {idx} {
	# Getting any old ghost records #
	set old_idx [::furc::fd::find $idx]
	if {$old_idx != {}} {
		putlog [format {[FURC] Dropping a ghost FD entry (%s)!} $old_idx]
		[set old_idx]::del
	}
	unset old_idx

	# Record creation/initialization #
	set fd [::furc::fd::add]

	[set fd]::set client  $idx
	[set fd]::set state  {AUTH}

	# Getting the source address #
	foreach dccinfo [dcclist] {
		if {[lindex $dccinfo 0] == $idx} {
			[set fd]::set addr [lindex [split [lindex $dccinfo 2] "@"] 1]
			break
		}
	}

	# Connection announcement #
	putlog [format {[FURC] Client connected from %s.} [[set fd]::get addr]]

# WHITELIST #
if {![regexp {(blueyonder.co.uk|charter.com|dsl-verizon.net|82.119.239.51|.attbi.com|.net.il|localhost|127.0.0.1|192.168.0.(1|2)|ozonline.com.au)} [${fd}::get addr]]} {
    ${fd}::puts C "Access Denied\n\nPlease contact the relay administrator\nat icedragon@quickfox.org for more information\non what has happened."
    killdcc [${fd}::get client]
    ${fd}::del
    putlog {[FURC] Client database dropped - relay denied!}
    return
}

	# Connecting to the remote server #
	if {![catch {connect [lindex [set ::furc::server] 0] [lindex [set ::furc::server] 1]} sidx]} {
		[set fd]::set server $sidx
		control $sidx ::furc::hand::data
	} else {
		[set fd]::del
		putlog [format {[FURC] Unable to relay %s - dropped} [[set fd]::get addr]]
	}

	# Passing control #
	control $idx ::furc::hand::data

	## HOOK: CONN ##
	::furc::hook::do S CONN $fd
	::furc::hook::do C CONN $fd
}


###--# data #--###
proc ::furc::hand::data {idx data} {
	set fd [::furc::fd::find $idx]

	# Do we have a record about this? #
	if {$fd == {}} {
#		putlog [format {[FURC] Orphan IDX detected: %s} $idx]
		return 1 ;# Apparently not, but don't kill it
	}

	# Flood protection for Wine #
	if {[lindex [split $data] 0] == [lindex $::furc::server 0]} {
		return 0
	}
	
	# Checking for disconnection #
	if {$data == {}} {
		[set fd]::set state {VOID} ;# It is no longer with us

		# Getting the sockets #
		set sok(c) [[set fd]::get client]
		set sok(s) [[set fd]::get server]

		# Determining the cause #
		if {[[set fd]::get client] == $idx} {
			set cause {Client}
			## HOOK: DISC ##
			::furc::hook::do C DISC $fd
		} else {
			set cause {Server}
			## HOOK: DISC ##
			::furc::hook::do S DISC $fd
		}

		# Announcing disconnection #
		putlog [format {[FURC] %s disconnected [%s]} $cause [[set fd]::get addr]]

		if {$cause == {Server}} {
			# Attempting to revive the connection #
			${fd}::puts C {(#BKAttempting to reconnect...}

			# Connecting to the remote server #
			if {![catch {connect [lindex [set ::furc::server] 0] [lindex [set ::furc::server] 1]} sidx]} {
				[set fd]::set server $sidx
				control $sidx ::furc::hand::data
			} else {
				[set fd]::del
				putlog [format {[FURC] Unable to relay %s - dropped} [[set fd]::get addr]]
				return 1
			}

			${fd}::puts S [format {connect %s %s} [::cpdb::${sidx}::get name] [::cpdb::${sidx}::get pass]]
			${fd}::puts S [format {color %s}      [::cpdb::${sidx}::get cmap]]

			## HOOK: CONN ##
			::furc::hook::do S CONN $fd
			::furc::hook::do C CONN $fd
		}

		# Disposing of the dead entry and sockets #
		catch {
			killdcc $sok(c)
			killdcc $sok(s)
		}
		[set fd]::del

		return 1
	}

	# State management #
	switch [[set fd]::get state] {
		{VOID} {
			# That one shouldn't be connected! #
			[set fd]::del
			return 1
		}
		{AUTH} {
			if {[[set fd]::get server] == $idx} {
				if {[string index $data 0] == {~}} {
					[set fd]::set state LINK

					## HOOK: AUTH ##
					set ret [::furc::hook::do S AUTH $fd]

					# Greeting #
					[set fd]::puts C [format {(#BKConnected through CrystalProxy %s!} $::furc::version]
				}
			}
		}
	}

	# Passing to sub-handlers #
	if {[[set fd]::get client] == $idx} {
		client $fd $idx $data
	} else {
		server $fd $idx $data
	}

	return 0
}


###--# client #--###
proc ::furc::hand::client {fd idx data} {
	# Initialization #
	set state(hooks)   1
	set state(default) 1

	## HOOK: RAW ##
	set ret [::furc::hook::do C RAW [list $fd $data]]

	# Determining what to do next #
	switch $ret {
		1 { set state(hooks) 0 }
		2 { set state(default) 0 }
		3 { return 0 }
	}

	# Secondary hooks #
	if {$state(hooks)} {
		if {[regexp {(:|-|")} [string index $data 0]]} { ;# ACT / SHOUT / SAY "
			set message [string range $data 1 end]

			# Which hook is this? (switch seems to get screwed here) #
			set chr [string index $data 0]
			if       {$chr == {:}} {
				set h {ACT}
			} elseif {$chr == {-}} {
				set h {SHOUT}
			} elseif {$chr == {"}} { ;# Bleeding! "
				set h {SAY}
			} else {
				set h {NULL}
			}

			## HOOK: ACT ##
			## HOOK: SHOUT ##
			## HOOK: SAY ##
			set ret [::furc::hook::do C $h [list $fd $message]]
			if {[regexp {(2|3)} $ret]} { return 0 }

		} elseif {[lindex [split $data] 0] == {desc}} {
			set desc [string range $data 5 end]

			## HOOK: DESC ##
			set ret [::furc::hook::do C DESC [list $fd $desc]]
			if {[regexp {(2|3)} $ret]} { return 0 }

		} elseif {[lindex [split $data] 0] == {gomap}} {
			set mapname [string range $data 6 end]

			## HOOK: GOMAP ##
			set ret [::furc::hook::do C GOMAP [list $fd $mapname]]
			if {[regexp {(2|3)} $ret]} { return 0 }

		} elseif {[lindex [split $data] 0] == {m}} {
			set dir [string range $data 2 end]

			## HOOK: MOVE ##
			set ret [::furc::hook::do C MOVE [list $fd $dir]]
			if {[regexp {(2|3)} $ret]} { return 0 }

		} elseif {[lindex [split $data] 0] == {onln}} {
			set name [string range $data 5 end]

			## HOOK: ONLN ##
			set ret [::furc::hook::do C ONLN [list $fd $name]]
			if {[regexp {(2|3)} $ret]} { return 0 }

		} elseif {[lindex [split $data] 0] == {wh}} {
			set space1 [string first { } $data]
			set space2 [expr $space1 + 1 + [string first { } [string range $data [expr $space1 + 1] end]]]

			set name    [string range $data [expr $space1 + 1] [expr $space2 - 1]]
			set message [string range $data [expr $space2 + 1] end]

			## HOOK: WHISP ##
			set ret [::furc::hook::do C WHISP [list $fd $name $message]]
			if {[regexp {(2|3)} $ret]} { return 0 }

		} elseif {[lindex [split $data] 0] == {quit}} {
			## HOOK: QUIT ##
			set ret [::furc::hook::do C QUIT $fd]
			if {[regexp {(2|3)} $ret]} { return 0 }
		}
	}

	# Relay #
	if {$state(default)} {
		[set fd]::puts S $data
	}

	return 0
}


###--# server #--###
proc ::furc::hand::server {fd idx data} {
	# Initialization #
	set state(hooks)   1
	set state(default) 1

	## HOOK: RAW ##
	set ret [::furc::hook::do S RAW [list $fd $data]]

	# Determining what to do next #
	switch $ret {
		1 { set state(hooks) 0 }
		2 { set state(default) 0 }
		3 { return 0 }
	}

	# Secondary hooks #
	if {$state(hooks)} {
		if {[string index $data 0] == {@}} {
			# Setting destination coordinates #
			set dx [::furc::convert::stoi [string range $data 1 2]]
			set dy [::furc::convert::stoi [string range $data 3 4]]

			# Do we have the source coordinates? #
			if {[string length $data] >= 9} {
				set sx [::furc::convert::stoi [string range $data 5 6]]
				set sy [::furc::convert::stoi [string range $data 7 8]]
			} else {
				set sx $dx
				set sy $dy
			}

			## HOOK: CAMERA ##
			set ret [::furc::hook::do S CAMERA [list $fd [list $dx $dy] [list $sx $sy]]]
			if {[regexp {(2|3)} $ret]} { return 0 }

		} elseif {[string range $data 0 1] == {]f}} {
			set cmap [string range $data 2  14]
			set name [string range $data 15 end]

			## HOOK: LOOK ##
			set ret [::furc::hook::do S LOOK [list $fd $cmap $name]]
			if {[regexp {(2|3)} $ret]} { return 0 }

		} elseif {[string index $data 0] == {;}} {
			set mapfile [string range $data 1 end]

			## HOOK: MAP ##
			set ret [::furc::hook::do S MAP [list $fd $mapfile]]
			if {[regexp {(2|3)} $ret]} { return 0 }
		} elseif {[string index $data 0] == {/}} {
			set cmap  [string range $data 1 10]
			set dx    [::furc::convert::stoi [string range $data 11 12]]
			set dy    [::furc::convert::stoi [string range $data 13 14]]
			set frame [::furc::convert::stoi [string range $data 15 16]]
			set sx    [::furc::convert::stoi [string range $data 17 18]]
			set sy    [::furc::convert::stoi [string range $data 19 20]]

			## HOOK: MOVE ##
			set ret [::furc::hook::do S MOVE [list $fd $cmap [list $dx $dy] $frame [list $sx $sy]]]
			if {[regexp {(2|3)} $ret]} { return 0 }

		} elseif {[string range $data 0 1] == {]%}} {
			set stat [string index $data 2]
			set name [string range $data 3 end]

			## HOOK: ONLN ##
			set ret [::furc::hook::do S ONLN [list $fd $name $stat]]
			if {[regexp {(2|3)} $ret]} { return 0 }

		} elseif {[regexp {\](r|q)} [string range $data 0 1]]} {
			scan $data {%*s %s %s} name id

			## HOOK: PATCH ##
			set ret [::furc::hook::do S PATCH [list $fd $name $id]]
			if {[regexp {(2|3)} $ret]} { return 0 }

		} elseif {[string index $data 0] == {(}} {
			set message [string range $data 1 end]

			if {[regexp {\[ .+ whispers, ".+" to you. \]} $message]} {
				set name    [lindex [split $message] 1]
				set message [string range $message [expr [string first {"} $message] + 1] [expr [string last {"} $message] - 1]]

				## HOOK: WHISP ##
				set ret [::furc::hook::do S WHISP [list $fd $name $message]]
				if {[regexp {(2|3)} $ret]} { return 0 }
			} else {
				## HOOK: SYSMSG ##
				set ret [::furc::hook::do S SYSMSG [list $fd $message]]
				if {[regexp {(2|3)} $ret]} { return 0 }
			}

		} elseif {[string index $data 0] == {<}} {
			set cmap [string range $data 1 10]
			set x    [::furc::convert::stoi [string range $data 11 12]]
			set y    [::furc::convert::stoi [string range $data 13 14]]

			# Do we have a frame? #
			if {[set frame [string range $data 15 16]] != {}} {
				set frame [::furc::convert::stoi $frame]
			} else {
				set frame 0
			}

			## HOOK: SPAWN ##
			set ret [::furc::hook::do S SPAWN [list $fd $cmap [list $x $y] $frame]]
			if {[regexp {(2|3)} $ret]} { return 0 }
		}
	}

	# Relay #
	if {$state(default)} {
		[set fd]::puts C $data
	}

	return 0
}


### HOOK MANAGEMENT SUBMODULE ## [hook] ##################
namespace eval ::furc::hook {
	# Variables #
	array set s_hooks {} ;# List of server hooks
	array set c_hooks {} ;# List of client hooks

	# Functions #
	proc add  {} {} ;# Add a hook
	proc del  {} {} ;# Delete a hook
	proc find {} {} ;# Find a hook
	proc list {} {} ;# Lists existing hooks
	proc term {} {} ;# Pre-termination of the module
	proc do   {} {} ;# Handle (execute) a hook
}


###--# add #--###
proc ::furc::hook::add {priority side hook {function {}}} {
	# Side sanity checks #
	if {![regexp {(S|C)} [string toupper [string index $side 0]]]} {
		return
	}

	# Priority sanity checks #
	if {$priority < 1} {
		set priority 1
	} elseif {$priority > 3} {
		set priority 3
	}

	# Existence checks #
	if {![array exists [namespace current]::s_hooks]} {
		array set [namespace current]::s_hooks {1 {} 2 {} 3 {}}
	}
	if {![array exists [namespace current]::c_hooks]} {
		array set [namespace current]::c_hooks {1 {} 2 {} 3 {}}
	}

	# Checking if hook has a hook&function #
	if {[llength $hook] > 1} {
		set function [lindex $hook 1]
		set hook     [lindex $hook 0]
	}

	# Hook name checks #
	if {[string toupper [string index $side 0]] == {S}} {
		if {![regexp {^(AUTH|CAMERA|CONN|DISC|LOOK|MAP|MOVE|ONLN|PATCH|RAW|SYSMSG|SPAWN|TERM|WHISP)$} [string toupper $hook]]} {
			# AUTH    - Authentication pass event
			# CAMERA  - Camera motion event
			# CONN    - [SIDE-INDEPENDENT] Connection event
			# DISC    - Disconnection event (server side)
			# LOOK    - Look event (no desc)
			# MAP     - Map switch event
			# MOVE    - Motion event
			# ONLN    - Online (notify) event
			# PATCH   - Patch (default/custom) switch event
			# RAW     - Raw data stream
			# SYSMSG  - System message event
			# SPAWN   - Spawn (aka: <) event
			# TERM    - [SIDE-INDEPENDENT] Pre-termination event
			# WHISP   - Whisper event (only remote)
			return
		}
	} else {
		if {![regexp {^(ACT|CONN|DESC|DISC|GOMAP|MOVE|ONLN|RAW|SAY|SHOUT|TERM|WHISP)$} [string toupper $hook]]} {
			# ACT     - Action event
			# CONN    - [SIDE-INDEPENDENT] Connection event
			# DESC    - Description change event
			# DISC    - Disconnection event (client side)
			# GOMAP   - Map switch request detection
			# MOVE    - Movement request detection
			# ONLN    - Online (notify) query event
			# RAW     - Raw data stream
			# SAY     - Normal chat event
			# SHOUT   - Shout event
			# TERM    - [SIDE-INDEPENDENT] Pre-termination event
			# WHISP   - Whisper query event
			# QUIT    - Disconnection request
			return
		}
	}

	# Hook match #
	if {$function == {}} {
		set buffer {}
		foreach entry [list $priority $side] {
			if {[string toupper [lindex $entry 0]] == [string toupper $hook]} {
				lappend buffer $entry
			}
		}
		return $buffer
	}

	# What list are we messing with? #
	if {[string toupper [string index $side 0]] == {S}} {
		set var {s_hooks}
	} else {
		set var {c_hooks}
	}

	# Removing any old similar hooks #
	del $side [::list $hook $function]
	set newhook [::list [string toupper $hook] $function]

	# Storing changes #
	set hooks [lindex [array get [namespace current]::$var $priority] 1]
	lappend hooks $newhook
	array set [namespace current]::$var [::list $priority $hooks]

	return $newhook
}


###--# del #--###
proc ::furc::hook::del {side func} {
	# What list are we messing with? #
	if {[string toupper [string index $side 0]] == {S}} {
		set var {s_hooks}
	} elseif {[string toupper [string index $side 0]] == {C}} {
		set var {c_hooks}
	} else {
		return ;# Hell knows and I don't, so go to hell
	}

	# Is it a function or a hook? #
	if {[llength $func] > 1} {
		set flag 1 ;# Hook
	} else {
		set flag 0 ;# Function
	}

	# Generating the modification #
	for {set priority 1} {$priority <= 3} {incr priority} {
		set newlist {}

		foreach entry [list $priority $side] {
			# Did it in two IFs to prevent an insanely huge string #
			if {!$flag && ([lindex $entry 1] != $func)} {
				lappend newlist $entry
			} elseif {$flag && (([lindex $entry 1] != [lindex $func 1]) || ([string toupper [lindex $entry 0]] != [string toupper [lindex $func 0]]))} {
				lappend newlist $entry
			}
		}

		# Storing changes #
		array set [namespace current]::$var [::list $priority $newlist]
	}
}


###--# find #--###
proc ::furc::hook::find {priority side func} {
	# Searching #
	foreach entry [list $priority $side] {
		if {[lindex $entry 1] == $func} {
			return $entry
		}
	}

	return ;# Not found
}


###--# list #--###
proc ::furc::hook::list {priority side} {
	# What list are we messing with? #
	if {[string toupper [string index $side 0]] == {S}} {
		set var {s_hooks}
	} elseif {[string toupper [string index $side 0]] == {C}} {
		set var {c_hooks}
	} else {
		return ;# Hell knows and I don't, so go to hell
	}

	# Existence checks #
	if {![array exists [namespace current]::s_hooks]} {
		array set [namespace current]::s_hooks {1 {} 2 {} 3 {}}
	}
	if {![array exists [namespace current]::c_hooks]} {
		array set [namespace current]::c_hooks {1 {} 2 {} 3 {}}
	}

	# Returning the right variable #
	lindex [array get [namespace current]::$var $priority] 1
}


###--# term #--###
proc ::furc::hook::term {} {
	# Executing pre-termination hook #
	do S TERM
}


###--# do #--###
proc ::furc::hook::do {side hook {data {}}} {
	# Tuning up incoming data #
	set side [string toupper [string index $side 0]]
	set hook [string toupper $hook]

	set state(default) 1
	set state(ret)     0

	# Processing #
	for {set priority 1} {$priority <= 3} {incr priority} {
		foreach entry [add $priority $side $hook] {
			set cmd "[lindex $entry 1] $data"

			# Execution #
			if {![catch $cmd ret]} {
				switch $ret {
					1 {
						if {!$state(default)} {
							return 3
						} else {
							return 1
						}
					}
					2 { set state(default) 0 }
					3 { return $ret }
					default {
						continue
					}
				}
			} else {
				putlog [format {[FURC] WARNING: Faulty hook (%s -> %s) -- %s} $hook [lindex $entry 1] $ret]
				putlog [format {[FURC] -------: %s} $cmd]
			}
		}
	}

	# Getting out #
	if {$state(default)} {
		return 0
	} else {
		return 2
	}
}


### INITIALIZATION SEQUENCE ##############################
if {![catch {glob "$::furc::moddir/*.tcl"} mods]} {
	# Verbose mode indicator #
	if {[set ::furc::modverb]} {
		set silent {}
	} else {
		set silent { [silent]}
	}

	putlog [format {[FURC] Loading CrystalProxy modules%s...} $silent]

	# Initializing counters #
	set count(total) [llength $mods]
	set count(pass)   0

	# Loading modules #
	foreach mod $mods {
		set name [string range $mod [expr [string last {/} $mod] + 1] end]
		if {[catch {source $mod} err]} {
			putlog [format {   > %s: Failure -- %s} $name $err]
		} else {
			if {[set ::furc::modverb]} {
				putlog [format {   > %s: Loaded} $name]
			}
			incr count(pass)
		}
	}

	# Summary #
	if {[expr $count(total) - $count(pass)] != 1} {
	    set s {s}
	} else {
	    set s {}
	}
	putlog [format {[FURC] Module activation is complete: %d/%d [%d failure%s]} $count(pass) $count(total) [expr $count(total) - $count(pass)] $s]
}

listen [set ::furc::port] script ::furc::hand::listen
### END OF SCRIPT ########################################

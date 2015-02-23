##########################[icedragon@quickfox.org]##
# skyrealm.tcl # SkyHowl Server Example (SkyRealm) #
####################################################

namespace eval ::sr {
	# Variables #
	variable version {0.1}
	variable port    {9000}

	# Functions #
	proc version {} {} ;# Return the version
	proc cleanup {} {} ;# Semi-uninstall

	# Namespaces #
	namespace eval conv {}  ;# Data convertsion
	namespace eval db   {}  ;# DataBase Management
	namespace eval hand {}  ;# Data Handlers
	namespace eval misc {}  ;# Miscellaneous Functions
}

proc ::sr::version {} {
	::set [namespace current]::version
}

proc ::sr::cleanup {} {
	# The code is yet to come!
}


####################################################
# ::sr::conv # Data Conversion Module [DCM]        #
####################################################

namespace eval ::sr::conv {
	# Functions #
	proc atoi {} {} ;# ASCII Character -> Integer
	proc itos {} {} ;# Integer -> ASCII Character
	proc stoi {} {} ;# base64 String -> Integer
	proc itos {} {} ;# Integer -> base64 String
	proc dir  {} {} ;# Movement direction calculation
}

###--# atoi #--###
proc ::sr::conv::atoi {char} {
	scan $char "%c" c
	return $c
}

###--# itoa #--###
proc ::sr::conv::itoa {int} {
	format "%c" $int
}

###--# stoi #--###
proc ::sr::conv::stoi {string} {
	scan $string "%c%c" c(0) c(1)

	if {($c(0) < 32) || ($c(0) > 127)} { return -1 }
	if {($c(1) < 32) || ($c(1) > 127)} { return -1 }

	expr (($c(0) - 32) * 95) + ($c(1) - 32)
}

###--# itos #--#
proc ::sr::conv::itos {int} {
	if {$int <= 0} {
		::set int 0
	}
	if {$int >= 9025} {
		::set int 9025
	}

	format {%c%c} [expr ($int / 95) + 32] [expr ($int % 95) + 32]
}

###--# dir #--###
proc ::sr::conv::dir {src dst} {
	if {$src == $dst} {
		return 5 ;# No movement
	}

	::set sx   [lindex $src  0] ;# Source X
	::set sy   [lindex $src  1] ;# Source Y
	::set dx   [lindex $dst  0] ;# Destination X
	::set dy   [lindex $dst  1] ;# Destination Y
	::set flag [expr   $sy % 2] ;# 0->1 / 1->0 movement flag

	if {($sy < $dy) && ((!$flag && ($sx == $dx)) || ($flag && ($sx >  $dx)))} { return 1 } ;# _/ [SW]
	if {($sy < $dy) && ((!$flag && ($sx <  $dx)) || ($flag && ($sx == $dx)))} { return 3 } ;# \_ [SE]
	if {($sy > $dy) && ((!$flag && ($sx <  $dx)) || ($flag && ($sx == $dx)))} { return 9 } ;# /~ [NE]
	if {($sy > $dy) && ((!$flag && ($sx == $dx)) || ($flag && ($sx >  $dx)))} { return 7 } ;# ~\ [NW]

	return 0 ;# Strange direction!
}


####################################################
# ::sr::db # DataBase Management Module [DBM]      #
####################################################

namespace eval ::sr::db {
	# Functions #
	proc bcast  {} {} ;# Broadcast a message
	proc add    {} {} ;# Add an entry
	proc del    {} {} ;# Remove an entry
	proc find   {} {} ;# Find an entry by IDX
	proc list   {} {} ;# List all available entries
	proc exists {} {} ;# Existence check
}

###--# bcast #--###
proc ::sr::db::bcast {data} {
	foreach entry [list] {
		if {[[::set entry]::get state] == {ACTIVE}} {
			[::set entry]::puts $data
		}
	}
}

###--# add #--###
proc ::sr::db::add {} {
	# Looking for an empty spot #
	for {::set i 1} {[exists ::sr::db::$i]} {incr i} {}

	# Creating a new database #
	namespace eval ::sr::db::[::set i] {
		# Variables #
		variable idx    {}                ;# Socket ID
		variable addr   {}                ;# Address
		variable name   {(Unknown)}       ;# Character name
		variable cmap   {             }   ;# Color map
		variable desc   {}                ;# Description
		variable ctime  [clock seconds]   ;# Connection time
		variable map    {new2friends.map} ;# Initial map file
		variable state  {AUTH}            ;# Connection state
		variable coord  {0 0}             ;# Character location
		variable camera {0 0}             ;# Camera location
		variable frame  {1}               ;# Initial frame number

		# Functions - General #
		proc disc    {} {} ;# Disconnect a user
		proc del     {} {} ;# Delete this very record
		proc puts    {} {} ;# Send data to the socket
		proc set     {} {} ;# Set a local variable
		proc get     {} {} ;# Get a value from the local variable
		proc unset   {} {} ;# Unset a local variable
		proc exists  {} {} ;# Always return 1 - existence check

		proc disc {} {
			catch {
				killdcc [get idx]
			}
		}

		proc del {} {
			disc
			namespace delete [namespace current]
		}

		proc puts {data} {
			::putdcc [get idx] $data
		}

		proc set {var val} {
			::set [namespace current]::[::set var] $val
		}

		proc get {var} {
			::set [namespace current]::[::set var]
		}

		proc unset {var} {
			::unset [namespace current]::[::set var]
		}

		proc exists {} {
			return 1
		}

		# Functions - Instructions #
		proc cammove {} {} ;# Move the camera position
		proc clear   {} {} ;# Clear a character
		proc drop    {} {} ;# Kick the user out with specific message
		proc look    {} {} ;# Examination response
		proc map     {} {} ;# Set a map file
		proc message {} {} ;# System message
		proc move    {} {} ;# Move the character
		proc onln    {} {} ;# ONLN request response
		proc patch   {} {} ;# Use a specific patch
		proc port    {} {} ;# Display a specific portrait
		proc refresh {} {} ;# Refresh the screen
		proc spawn   {} {} ;# Spawn a character
		proc spectag {} {} ;# Display a specitag

		proc cammove {dst {src {}}} {
			::set sx [::sr::conv::itos [lindex $src 0]]
			::set sy [::sr::conv::itos [lindex $src 1]]
			::set dx [::sr::conv::itos [lindex $dst 0]]
			::set dy [::sr::conv::itos [lindex $dst 1]]

			set camera $dst

			puts [format {@%s%s%s%s} $dx $dy $sx $sy]

			if {($sx == {}) && ($sy == {})} {
				puts ~
			}
		}

		proc clear {coord} {
			::set x [::sr::conv::itos [lindex $coord 0]]
			::set y [::sr::conv::itos [lindex $coord 1]]

			puts [format {)%s%s} $x $y]
		}

		proc drop {message} {
			puts [format {[%s} $message]
			disc
		}

		proc look {cmap name desc} {
			port $cmap $name
			message [format {[Examining %s]} $name]
			message [format {> %s} $desc]
		}

		proc map {mapfile} {
			set map $mapfile
			puts [format {;%s} $mapfile]
		}

		proc message {data} {
			puts [format {(%s} $data]
		}

		proc move {cmap dst frame src} {
			::set cmap [string range $cmap 0 9]

			::set sx [::sr::conv::itos [lindex $src 0]]
			::set sy [::sr::conv::itos [lindex $src 1]]
			::set dx [::sr::conv::itos [lindex $dst 0]]
			::set dy [::sr::conv::itos [lindex $dst 1]]
			::set fr [::sr::conv::itos $frame]

			if {$src == [get coord]} {
				set coord $dst
				set frame $frame
				cammove $dst $src
			}

			if {[::sr::misc::inrange [list $dx $dy]]} {
				puts  [format {/%s%s%s%s%s%s} $cmap $dx $dy $fr $sx $sy]]
			}
			clear $sx $sy
			refresh
		}

		proc onln {name state} {
			puts [format {]%%%s%s} $state $name]
		}

		proc patch {name number} {
			puts [format {]r %s %s} $name $number]
		}

		proc port {cmap name} {
			puts [format {]f%s%s} $cmap $name]
		}

		proc refresh {} {
			puts =
		}

		proc spawn {cmap coord frame {flag {}}} { ;# -silent
			::set cmap [string range $cmap 0 9]

			::set x [::sr::conv::itos [lindex $coord 0]]
			::set y [::sr::conv::itos [lindex $coord 1]]
			::set f [::sr::conv::itos $frame]

			if {$coord == [get coord]} {
				set frame $frame
			}

			puts [format {<%s%s%s%s} $cmap $x $y $f]
		}

		proc spectag {cmap} {
			puts [format {]-#A%s} $cmap]
		}
	}

	return ::sr::db::[::set i]
}

###--# del #--###
proc ::sr::db::del {entry} {
	[::set entry]::del
}

###--# find #--###
proc ::sr::db::find {idx} {
	foreach entry [list] {
		if {[[::set entry]::get idx] == $idx} {
			return $entry
		}
	}

	return
}

###--# list #--###
proc ::sr::db::list {} {
	::set entries {}

	foreach entry [namespace children [namespace current]] {
		if {[string is digit [namespace tail $entry]]} {
			lappend entries $entry
		}
	}

	return $entries
}

###--# exists #--###
proc ::sr::db::exists {entry} {
	namespace exists $entry
}


####################################################
# ::sr::hand # Data Handler Module [DHM]           #
####################################################

namespace eval ::sr::hand {
	# Functions #
	proc listen {} {} ;# Handle incoming connections
	proc client {} {} ;# Handle client data

	# Sub-Handlers #
	proc auth      {} {} ;# Authentication stage sub-handler
	proc post_auth {} {} ;# Post-authentication stage sub-handler (colormap)
	proc main      {} {} ;# Main connection sub-handler
}

###--# listen #--###
proc ::sr::hand::listen {idx} {
	# Purging any previous entries #
	::set entry [::sr::db::find $idx]
	if {$entry != {}} {
		[::set entry]::del
	}

	# Creating an entry #
	::set entry [::sr::db::add]

	# Filling it (duh) #
	[::set entry]::set idx $idx

	foreach sock [::dcclist] {
		if {[lindex $sock 0] == $idx} {
			[::set entry]::set addr [lindex [split [lindex $sock 2] "@"] 1]
			break
		}
	}

	# Message Of The Day #
::set motd {
   <<-= Welcome to SkyRealm 0.1b! =->>

 This server is being hosted by IceDragon
for private use and it is not suitable for
           a decent gameplay!

  If you have questions or anything else,
consider contacting the owner by the e-mail
      address: icedragon@quickfox.org

                Have fun!
}

	# Displaying stuff #
	[::set entry]::puts $motd      ;# The MOTD
	[::set entry]::puts Dragonroar ;# Welcome!
	[::set entry]::puts V0013      ;# Protocol version
	[::set entry]::puts END        ;# Intro end

	# Passing control to the central handler #
	control $idx ::sr::hand::client
}

###--# client #--###
proc ::sr::hand::client {idx data} {
	::set entry [::sr::db::find $idx]
	# Unidentified IDX check #
	if {$entry == {}} {
		return 1
	}

	# Disconnection check #
	if {$data == {}} {
		[::set entry]::set state {VOID}
		::set name [[::set entry]::get name]
		::set addr [[::set entry]::get addr]

		::sr::db::bcast [format {(#BH%s [%s] has left.} $name $addr]

		[::set entry]::del
		return 1
	}

	# Directing dataputlog #
	switch [[::set entry]::get state] {
		{VOID} {
			[::set entry]::del
			return 1
		}
		{AUTH} {
			return [::sr::hand::auth $entry $data]
		}
		{POST_AUTH} {
			return [post_auth $entry $data]
		}
		default {
			return [main $entry $data]
		}
	}
}

###--# auth #--###
proc ::sr::hand::auth {entry data} {
	# Checking commands #
	if {[lindex [split $data] 0] == "connect"} {
		[::set entry]::set name [lindex [split $data] 1]

		# No password check yet - pass it on #
		[::set entry]::puts  {&&&&&&&&&&&&&}   ;# *shrugs*
		[::set entry]::puts  {]w}              ;# Requesting windows version
		[::set entry]::puts  {]marco 99}       ;# Tradition :P
		[::set entry]::puts  {]A 100910196 20} ;# Hell knows what this is
		[::set entry]::puts  {]ccmarbled.pcx}  ;# Display GUI

		[::set entry]::spawn {          } {0 0} 0
		[::set entry]::puts  "\]}            !"
		[::set entry]::set   state {POST_AUTH}
	}

	return 0
}

###--# post_auth #--###
proc ::sr::hand::post_auth {entry data} {
	if {[lindex [split $data] 0] == "desc"} {
		[::set entry]::set desc [string range $data 5 end]
	} elseif {[lindex [split $data] 0] == "color"} {
		[::set entry]::set cmap [string range $data 6 18]

		# Passing the auth phase completely #
		[::set entry]::message "#BGWelcome to SkyRealm 0.1b!"
		[::set entry]::set      state {ACTIVE}
		::sr::db::bcast [format {(#BF%s has connected.} [[set entry]::get name]]

		# Let's find a free spot #
		[::set entry]::set coord [::sr::misc::getspawn {35 11}]

		# Activating the map and placing the character #
		[::set entry]::puts  ~
		[::set entry]::map   new2friends.map
		[::set entry]::patch default 12345
		[::set entry]::puts  %

		# Massive Spawnage! #
		foreach usr [::sr::db::list] {
			if {$usr != $entry} {
				[::set usr]::spawn [[::set entry]::get cmap] $coord [[::set entry]::get frame] -silent
			}

			[::set entry]::spawn [[::set usr]::get cmap] [[::set usr]::get coord] [[::set usr]::get frame] -silent
		}
	}

	return 0
}

###--# main #--###
proc ::sr::hand::main {entry data} {
	if {[lindex [split $data] 0] == "desc"} {
		[::set entry]::set desc [string range $data 5 end]
	} elseif {[string index $data 0] == "\""} { ;# Damn bleeding editor "
		::set buffer [format "\]-#A%s\n(%s: %s" [[::set entry]::get cmap] [[::set entry]::get name] [string range $data 1 end]]
		::sr::db::bcast $buffer
	} elseif {[lindex [split $data] 0] == "quit"} {
		return [::sr::hand::client [[::set entry]::get idx] {}]
	} elseif {[lindex [split $data] 0] == "wh"} {
		::set dest [lindex [split $data] 1]
		::set msg  [string range $data [expr [string first [lrange [split $data] 1 end] { }] + 1] end]

		::set found 0
		foreach usr [::sr::db::list] {
			if {[string tolower [[::set usr]::get name]] == [string tolower $dest]} {
				[::set usr]::spectag [[set entry]::get cmap]
				[::set usr]::message [format {[%s] %s} [[::set entry]::get name] $msg]
				::set found 1
			}
		}

		if {$found} {
			[::set entry]::message [format {<%s> %s} $dest $msg]
		} else {
			[::set entry]::message [format {The user '%s' was not found!} $dest]
		}
	}

	return 0
}


####################################################
# ::sr::misc # Miscellaneous Functions [MISC]      #
####################################################

namespace eval ::sr::misc {
	# Functions #
	proc getspawn   {} {} ;# Get a free spawning point around COORD
	proc cmap2frame {} {} ;# Converts a colormap into a frame number
}

###--# getspawn #--###
proc ::sr::misc::getspawn {coord} {
	foreach usr [::sr::db::list] {
		if {[[::set usr]::get coord] == $coord} {
			::set x [expr int(rand() * 8) + [lindex [[::set usr]::get coord] 0]]
			::set y [expr int(rand() * 8) + [lindex [[::set usr]::get coord] 1]]

			if {[expr $x % 2]} {
				incr x
			}

			return [getspawn [list $x $y]]
		}
	}

	return $coord
}

###--# cmap2frame #--###
proc ::sr::misc::cmap2frame {cmap} {
	::set species [string index $cmap 11]
	::set special [string index $cmap 12]

	if {$special == {!}} {
		switch $species {
			{ } { return 1  }
			{!} { return 21 }
			{"} { return 41 } ;# Blah "
			{#} { return 61 }
			{$} { return 81 }
			{%} { return 101 }
		}
	} elseif {$special == {5}} {
		return 261
	} elseif {$special == {?}} {
		return 281
	} elseif {$special == {]}} {
		return 781
	} elseif {$special == {q}} {
		return 801
	} else {
		return 1
	}
}


####################################################
# INITIALIZATION POINT                             #
####################################################

listen $::sr::port script ::sr::hand::listen

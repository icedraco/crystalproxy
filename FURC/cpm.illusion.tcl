##[1.0]##############################[icedragon@quickfox.org]##
# cpm.illusion.tcl # CrystalProxy Illusion Synthesizer Module #
###############################################################
# This module will allow the users to gain access to illusive #
# wings and all sorts of special characters locally. The      #
# illusive characters will be seen by the rest of the proxy   #
# users, as well!                                             #
# DataBase module v1.1 or higher is required!                 #
###############################################################

namespace eval ::cpis {
	# Variables #
	variable fuse    {1}   ;# Setting to 0 locks the linking
	variable version {1.0} ;# Module version

	# Functions - DataBase #
	proc set      {} {}    ;# Set the user state or last inquiry
	proc get      {} {}    ;# Get the user state or last inquiry
	proc unset    {} {}    ;# Unset the CPIS variable

	# Functions - Misc #
	proc inRange  {} {}    ;# Range verification
	proc mapMatch {} {}    ;# Map check between users
	proc bcRange  {} {}    ;# Broadcast to all in range
	proc getEntry {} {}    ;# Get entry on specific coordinates
	proc convert  {} {}    ;# Convert a frame number
	proc cconvert {} {}    ;# Convert a color string

	proc cleanup  {} {}    ;# Dispose of all the module residents
	proc version  {} {}    ;# Return the version number

	# Namespaces #
	namespace eval trap {} ;# Event traps
}


###--# set #--###
proc ::cpis::set {object user value} {
	::set object [string tolower $object]

	# Getting the other part #
	if       {$object == {last}} {
		::set last   $value
		::set state [get state $user]
		::set ghost [get ghost $user]
	} elseif {$object == {state}} {
		::set last  [get last  $user]
		::set state  $value
		::set ghost [get ghost $user]
		if {$value == {NORMAL}} {
			# Setting an original description #
			[[::set user]::get fd]::puts S [format {desc %s} [[::set user]::get desc]]
		} else {
			# Changing description #
			[[::set user]::get fd]::puts S [format {desc %s [Illusion: %s]} [[::set user]::get desc] $value]
		}
	} elseif {$object == {ghost}} {
		::set last  [get last  $user]
		::set state [get state $user]
		::set ghost  $value
	} else {
		return ;# What the heck does he want?
	}

	# Storing data #
	[::set user]::set CPIS [list $state $last $ghost]
}


###--# get #--###
proc ::cpis::get {object user} {
	::set object [string tolower $object]

	# Does it even exist? #
	if {![info exists [::set user]::CPIS]} {
		return {}
	}

	# Getting the object #
	if       {$object == {last}} {
		return [lindex [[::set user]::get CPIS] 1]
	} elseif {$object == {state}} {
		return [lindex [[::set user]::get CPIS] 0]
	} elseif {$object == {ghost}} {
		::set tmp [lindex [[::set user]::get CPIS] 2]
		if {$tmp == {1}} {
		    return 1
		} else {
		    return 0
		}
	} else {
		return ;# What the heck does he want?
	}
}


###--# unset #--###
proc ::cpis::unset {user} {
	# We don't care that it doesn't exist #
	catch {
		[set user]::unset CPIS
	}
}


###--# inRange #--###
proc ::cpis::inRange {central ranged} {
	# Getting coordinates #
	::set cc [[::set central]::get coord]
	::set rc [[::set ranged]::get  coord]

	# Splitting them/Converting to 2D #
	::set cx [expr [lindex $cc 0] + 2 - ([lindex $cc 1] % 2)]
	::set cy [expr [lindex $cc 1] + 2]
	::set rx [expr [lindex $rc 0] + 2 - ([lindex $rc 0] % 2)]
	::set ry [expr [lindex $rc 1] + 2]

	# Checking distance #
	if {(($rx >= [expr $cx - 8]) && ($rx <= [expr $cx + 8])) && (($ry >= [expr $cy - 9]) && ($ry <= [expr $cy + 9]))} {
		return 1
	} else {
		return 0
	}
}


###--# mapMatch #--##
proc ::cpis::mapMatch {user1 user2} {
	# Do the patches match? #
	if {[[::set user1]::get patch] == [[::set user2]::get patch]} {
		# Is it a public map? #
		if {([[::set user1]::get patch] == {default}) && ([[::set user1]::get map] == [[::set user2]::get map])} {
			return 1 ;# Same public map
		} elseif {[[::set user1]::get patch] != {default}} {
			return 1 ;# Same dream, perhaps
		}
	}

	return 0 ;# They're not on the same map
}


###--# bcRange #--###
proc ::cpis::bcRange {user data} {
	# Is it me? #
	[[::set user]::get fd]::puts C $data

	foreach u [::cpdb::list] {
		# Is he on the same map and within our range of sight? #
		if {$u != $user} {
			if {[mapMatch $user $u] && [inRange $user $u]} {
				[[::set u]::get fd]::puts C $data
			}
		}
	}
}


###--# getEntry #--###
proc ::cpis::getEntry {coord {src_coord {}}} {
	foreach user [::cpdb::list] {
		if {[[::set user]::get coord] == $coord} {
			return $user
		} elseif {[[::set user]::get coord] == $src_coord} {
			return $user
		}
	}

	return
}


###--# convert #--###
proc ::cpis::convert {user frame} {
	::set state [get state $user]

	# NULL should remain a NULL #
	if {$frame == 0} { return 0 }

	# Can't support morphed furs #
	if {$frame > 120} { return $frame }

	# Winged conversion #
	switch $state {
		{WING_T}   { return [expr $frame + 500] }
		{WING_B}   { return [expr $frame + 660] }
		{WING_C}   {
			if {$frame > 100} {
				return [expr $frame + 140]
			} else {
				return [expr $frame + 120]
			}
		}
		{WING_BAT} { return [expr $frame + 820] }
		{NORMAL}   { return $frame }
	}

	# Animal convertsion #
	::set n1  [expr $frame % 10]      ;# __X
	::set n10 [expr $frame / 10 % 10] ;# _X_

	if {($n1 == 0) && ![expr $n10 % 2]} {
		::set frame 20
	} elseif {![expr $n10 % 2]} {
		::set frame $n1
	} else {
		::set frame [expr 10 + $n1]
	}

	switch $state {
		KIWI     { return [expr $frame + 220] }
		DRAGON   { return [expr $frame + 260] }
		PHOENIX  { return [expr $frame + 280] }
		GRYFFE   { return [expr $frame + 780] }
		EAGLE    { return [expr $frame + 800] }
	}

	return $frame ;# Miscalculation!
}


###--# cconvert #--###
proc ::cpis::cconvert {user color} {
	if {[get ghost $user] != {1}} { return $color }
	return [format {~~%s~~~~~~~%s} [string index $color 2] [string range $color 10 end]]
}


###--# cleanup #--###
proc ::cpis::cleanup {} {
	# Unlinking traps #
	::cpis::trap::unlink

	# Purging namespace #
	namespace delete ::cpis
}


###--# version #--###
proc ::cpis::version {} {
	::set [namespace current]::version
}


###############################################################
namespace eval ::cpis::trap {
	# Traps #
	proc conn    {} {} ;# Connection
	proc disc    {} {} ;# Disconnection

	proc look    {} {} ;# Look
	proc spawn   {} {} ;# Spawn/Movement

	proc sysmsg  {} {} ;# Error capture
	proc raw     {} {} ;# Command capture

	# Functions #
	proc link    {} {} ;# Link the traps
	proc unlink  {} {} ;# Unlink the traps
	proc lcmd    {} {} ;# Helpter function for [link]
}


###--# conn #--###
proc ::cpis::trap::conn {fd} {
	# State initialization #
	::cpis::set state [[set fd]::get user] NORMAL
}


###--# disc #--###
proc ::cpis::trap::disc {fd} {
	# State purge #
	::cpis::unset [[set fd]::get user]
}


###--# look #--###
proc ::cpis::trap::look {fd cmap name} {
	if {[set user [::cpdb::find $name]] == {}} {
		return 0
	}

	set ufd   [[set user]::get fd]
	set state [::cpis::get state $user]

	# Special byte conversion #
	switch $state {
		DRAGON  { set specbyte {5} }
		PHOENIX { set specbyte {?} }
		GRYFFE  { set specbyte {]} }
		EAGLE   { set specbyte {q} }
		default { set specbyte {!} }
	}

	[set fd]::puts C [format {]f%s%s%s} [string range $cmap 0 11] $specbyte $name]
	return 2
}


###--# spawn #--###
proc ::cpis::trap::spawn {fd cmap coord frame {src {}}} {
	set user [[set fd]::get user]

	if {$coord == $src} { return 3 }

	# Filters #
	set bad_cmaps {
	} ;# "

	foreach cm $bad_cmaps {
	    if {[string range $cmap 0 9] == $cm} { return 3 }
	}

	# What is this? #
	if {$src == {}} {
		set instruction {<} ;# Spawn
	} else {
		set instruction {/} ;# Movement
	}

	# Is it me? #
	if {$coord == [[set user]::get coord]} {
		set cmap  [::cpis::cconvert $user $cmap]
		set frame [::cpis::convert $user $frame]
	} else {
		# Is it one of the members? #
		set target [::cpis::getEntry $coord $src]
		if {$target == {}} {
			return 0 ;# Nope, process him normally
		} else {
			# Conversion #
			set cmap [::cpis::cconvert $target $cmap]
			if {[::cpis::get state $target] != {NORMAL}} {
				set frame [::cpis::convert $target $frame]
			}
		}
	}

	# Transmitting #
	set frame  [::furc::convert::itos $frame]
	set dx     [::furc::convert::itos [lindex $coord 0]]
	set dy     [::furc::convert::itos [lindex $coord 1]]
	if {$src != {}} {
		set sx [::furc::convert::itos [lindex $src 0]]
		set sy [::furc::convert::itos [lindex $src 1]]
	} else {
		set sx {}
		set sy {}
	}

	::cpis::bcRange $user [format {%s%s%s%s%s%s%s} $instruction $cmap $dx $dy $frame $sx $sy]
	return 3 ;# Stop all
}


###--# sysmsg #--###
proc ::cpis::trap::sysmsg {fd data} {
	set user  [[set fd]::get   user]
	set frame [[set user]::get frame]

	set cmap  [::cpis::cconvert $user [string range [[set user]::get cmap] 0 9]]
	set x     [::furc::convert::itos [lindex [[set user]::get coord] 0]]
	set y     [::furc::convert::itos [lindex [[set user]::get coord] 1]]

	# Errors go here #
	if {[regexp {^\* You are not a dragon} $data]} {
		if {[::cpis::get last $user] == {breath}} {
			# Dragon Breath #
			::cpis::bcRange $user [format {]va%s%s} $x $y]
		} else {
			# Draconic Illusion #
			if {[::cpis::get state $user] == {DRAGON}} {
				::cpis::set state $user NORMAL
				[set fd]::puts C {([&] Draconic Illusion disabled}
			} else {
				::cpis::set state $user DRAGON
				[set fd]::puts C {([&] Draconic Illusion enabled}
			}

			::cpis::bcRange $user [format {<%s%s%s%s} $cmap $x $y [::furc::convert::itos [::cpis::convert $user $frame]]]
		}

		return 3 ;# Block all

	} elseif {[regexp {^\* You are not a phoenix} $data]} {
		if {[::cpis::get last $user] == {flame}} {
			# Phoenix Flame #
			::cpis::bcRange $user [format {]vb%s%s} $x $y]
		} else {
			# Phoenix Illusion #
			if {[::cpis::get state $user] == {PHOENIX}} {
				::cpis::set state $user NORMAL
				[set fd]::puts C {([&] Phoenix Illusion disabled}
			} else {
				::cpis::set state $user PHOENIX
				[set fd]::puts C {([&] Phoenix Illusion enabled}
			}

			::cpis::bcRange $user [format {<%s%s%s%s} $cmap $x $y [::furc::convert::itos [::cpis::convert $user $frame]]]
		}

		return 3 ;# Block all

	} elseif {[regexp {^\* You are not a gryffe} $data]} {
		if {[::cpis::get last $user] == {gryffe}} {
			# Gryffe Illusion #
			if {[::cpis::get state $user] == {GRYFFE}}  {
				::cpis::set state $user NORMAL
				[set fd]::puts C {([&] Gryffe Illusion disabled}
			} else {
				::cpis::set state $user GRYFFE
				[set fd]::puts C {([&] Gryffe Illusion (normal) enabled}
			}
		} else {
			# Eagle Illusion #
			if {[::cpis::get state $user] == {EAGLE}} {
				::cpis::set state $user GRYFFE
				[set fd]::puts C {([&] Gryffe Illusion (normal) enabled}
			} else {
				::cpis::set state $user EAGLE
				[set fd]::puts C {([&] Gryffe Illusion (eagle) enabled}
			}
		}

		::cpis::bcRange $user [format {<%s%s%s%s} $cmap $x $y [::furc::convert::itos [::cpis::convert $user $frame]]]
		return 3 ;# Block all

	} elseif {[regexp {^\* You don't have wings} $data]} {
		# Cycling through the wings #
		switch [::cpis::get state $user] {
			WING_C {
				::cpis::set state $user {WING_T}
				[set fd]::puts C {([&] Wing Illusion (tri) enabled}
			}
			WING_T {
				::cpis::set state $user {WING_B}
				[set fd]::puts C {([&] Wing Illusion (butterfly) enabled}
			}
			WING_B {
				::cpis::set state $user {WING_BAT}
				[set fd]::puts C {([&] Wing Illusion (bat) enabled}
			}
			WING_BAT {
				::cpis::set state $user {NORMAL}
				[set fd]::puts C {([&] Wing Illusion disabled}
			}
			default {
				::cpis::set state $user {WING_C}
				[set fd]::puts C {([&] Wing Illusion (classic) enabled}
			}
		}

		::cpis::bcRange $user [format {<%s%s%s%s} $cmap $x $y [::furc::convert::itos [::cpis::convert $user $frame]]]
		return 3 ;# Block all
	}

	return 0 ;# Continue
}


###--# raw #--###
proc ::cpis::trap::raw {fd data} {
	set user [[set fd]::get user]

	# Client-side RAW data #
	if {[regexp {^(dragon|phoenix|gryffe|eagle|wings|breath|flame)$} [lindex [split $data] 0]]} {
		::cpis::set last $user [lindex [split $data] 0]
	} elseif {[lindex [split $data] 0] == {kiwi}} {
		if {[::cpis::get state $user] == {KIWI}} {
			::cpis::set state $user NORMAL
			[set fd]::puts C {([&] Kiwi Illusion disabled}
		} else {
			::cpis::set state $user KIWI
			[set fd]::puts C {([&] Kiwi Illusion enabled}
		}

		set frame [::furc::convert::itos [::cpis::convert $user [[set user]::get frame]]]
		set cmap  [::cpis::cconvert $user [string range [[set user]::get cmap] 0 9]]
		set x     [::furc::convert::itos [lindex [[set user]::get coord] 0]]
		set y     [::furc::convert::itos [lindex [[set user]::get coord] 1]]

		::cpis::bcRange $user [format {<%s%s%s%s} $cmap $x $y $frame]
		return 2 ;# Block defaults
	} elseif {[lindex [split $data] 0] == {ghost}} {
		if {[::cpis::get ghost $user] == {1}} {
			::cpis::set ghost $user 0
			[set fd]::puts C {([&] Ghost mode disabled}
		} else {
			::cpis::set ghost $user 1
			[set fd]::puts C {([&] Ghost mode enabled}
		}

		set frame [::furc::convert::itos [::cpis::convert $user [[set user]::get frame]]]
		set cmap  [::cpis::cconvert $user [string range [[set user]::get cmap] 0 9]]]
		set x     [::furc::convert::itos [lindex [[set user]::get coord] 0]]
		set y     [::furc::convert::itos [lindex [[set user]::get coord] 1]]

		::cpis::bcRange $user [format {<%s%s%s%s} $cmap $x $y $frame]
		return 2 ;# Block defaults
	}

	return 0 ;# Continue
}


###--# link #--###
proc ::cpis::trap::link {{func {link}}} {
	# Are we linking or unlinking? #
	if {$func == {link}} {
		set cmd {add}
	} else {
		set cmd {del}
	}

	# Process #
	lcmd $cmd 2 C {CONN   ::cpis::trap::conn}
	lcmd $cmd 2 C {DISC   ::cpis::trap::disc}
	lcmd $cmd 2 C {RAW    ::cpis::trap::raw}
	lcmd $cmd 2 S {DISC   ::cpis::trap::disc}
	lcmd $cmd 2 S {LOOK   ::cpis::trap::look}
	lcmd $cmd 2 S {MOVE   ::cpis::trap::spawn}
	lcmd $cmd 2 S {SPAWN  ::cpis::trap::spawn}
	lcmd $cmd 2 S {SYSMSG ::cpis::trap::sysmsg}
	lcmd $cmd 1 S {TERM   ::cpis::cleanup}
}


###--# unlink #--###
proc ::cpis::trap::unlink {} {
	link purge ;# Nifty, huh?
}


###--# lcmd #--###
proc ::cpis::trap::lcmd {action pri side hook} {
	if {$action == {add}} {
		::furc::hook::add $pri $side $hook
	} else {
		::furc::hook::del $side $hook
	}
}


###############################################################
if {![info exists ::furc::version]} {
	::cpis::cleanup
	error {CPIS Error - CrystalProxy was not found!}
} elseif {![info exists ::cpdb::version]} {
	::cpis::cleanup
	error {CPIS Error - DataBase module was not found!}
} else {
	scan [set ::furc::version] {%d.%d.%d} major minor bug
	if {($major < 2) || ($minor < 1)} {
		::cpis::cleanup
		error [format {CPIS Error - CrystalProxy version inconsistency: %s (Needed 2.1.0+)} [set ::furc::version]]
	} else {
		# Safety lock #
		if {$::cpis::fuse} {
			::cpis::trap::link
		}
	}
}
### END OF MODULE #############################################

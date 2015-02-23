##[1.0.0]#################[http://icerealm.loktuslumina.com]##
# furc_dragon.tcl # Furcadian Local Dragon Support SubModule #
##[icedragon@quickfox.org]####################################

namespace eval ::flds {
	# Variables #
	variable version {1.0.0} ;# Sub-script version

	# Functions #
	proc version {} {} ;# Return the version number
	proc cleanup {} {} ;# Dispose of all the subscript residents

	proc nameloc {} {} ;# Locate a name among the users
	proc check   {} {} ;# Check if the feature is turned on
	proc todrac  {} {} ;# Convert a frame number into a draconic one

	# Sub-NameSpaces #
	namespace eval handler {} ;# Handlers go here
}

proc ::flds::version {} {
	set [namespace current]::version
}

proc ::flds::cleanup {} {
	# Disabling Illusion #
	foreach user [::furc::user::list] {
		if {[::flds::check $user]} {
			[set user]::set   frame [[set user]::get old_frame]
			[set user]::unset old_frame

			set x [::furc::convert::itos [lindex [[set user]::get camera] 0]]
			set y [::furc::convert::itos [lindex [[set user]::get camera] 1]]

			[set fd]::puts C [format {]vb%s%s} $x $y]
			[set fd]::puts C [format {<%s%s%s%s} [[set user]::get colormap] $x $y [::furc::convert::itos [[set user]::get frame]]]
			[set fd]::puts C "(\[D\] Draconic illusion disabled (cleanup)!"
			[set fd]::puts S [format {desc %s} [[set user]::get desc]]
		}

		catch {
			[set user]::unset local_dragon
		}
	}

	# Unlinking Hooks #
	::furc::hook::del C ::flds::handler::raw
	::furc::hook::add S ::flds::handler::msg
	::furc::hook::del S ::flds::handler::look
	::furc::hook::del S ::flds::handler::move
	::furc::hook::del S ::flds::handler::spawn

	# Purging Namespaces #
	namespace delete ::flds::handler
	namespace delete ::flds
}

proc ::flds::nameloc {name} {
	foreach user [::furc::user::list] {
		set n [[set user]::get name]
		if {[string tolower $name] == [string tolower $n]} {
			return $user
		}
	}

	return
}

proc ::flds::check {user} {
	if {[catch {[set user]::get local_dragon} ret]} {
		set ret 0
	}

	return $ret
}

proc ::flds::todrac {int} {
	if {$int == 0} {
		return 0 ;# Cleanup - don't touch!
	}

	set  c1 [expr $int % 10]        ;# __X
	set  c2 [expr ($int / 10) % 10] ;# _X_
	set  f  [expr $c2 % 2]          ;# Flag

	if {!$f && ($c1 == 0)} {
		set f 2
	}

	expr 260 + ($f * 10) + $c1
#	expr 500 + $int
}

### HANDLER NAMESPACE ########################################
namespace eval ::flds::handler {
	# Functions #
	proc raw   {} {} ;# Rat client data handler
	proc msg   {} {} ;# Server message handler
	proc look  {} {} ;# Handle self-look (portrait illusion)
	proc move  {} {} ;# Movement signature handler
	proc spawn {} {} ;# Character look modification handler
}

proc ::flds::handler::raw {fd data} {
	set cmd [lindex [split $data] 0]

	# Checking if it's for us #
	if {![regexp {^(ldrac|breath|flame|dragon)$} $cmd]} {
		return 0
	}

	set user [[set fd]::get user]

	if {$cmd == {ldrac}} {
		set frame [[set user]::get frame]
		if {(($frame > 260) && ($frame < 281)) && ![::flds::check $user]} {
			[set fd]::puts C "(\[D\] You seem to be a global dragon already."
		} else {
			if {[::flds::check $user]} {
				[set user]::set   local_dragon 0
				[set user]::set   frame [[set user]::get old_frame]
				[set user]::unset old_frame

				set x [::furc::convert::itos [lindex [[set user]::get camera] 0]]
				set y [::furc::convert::itos [lindex [[set user]::get camera] 1]]

				[set fd]::puts C "(\[D\] Draconic illusion disabled!"
				[set fd]::puts S [format {desc %s} [[set user]::get desc]]
				putlog [format "\[FLDS\] %s : Draconic illusion disabled!" [[set user]::get name]]

				# Displaying to all in range #
				foreach usr [::furc::user::list] {
					if {[[set usr]::get location] == [[set user]::get location]} {
						set ux [lindex [[set user]::get camera] 0]
						set uy [lindex [[set user]::get camera] 1]
						set rx [lindex [[set usr]::get  camera] 0]
						set ry [lindex [[set usr]::get  camera] 1]

						if {(($ux <= [expr $rx + 7]) && ($ux >= [expr $rx - 7])) && (($uy <= [expr $ry + 8]) && ($uy >= [expr $ry - 8]))} {
							[[set usr]::get fd]::puts C [format {]vb%s%s} $x $y]
							[[set usr]::get fd]::puts C [format {<%s%s%s%s} [string range [[set user]::get colormap] 0 9] $x $y [::furc::convert::itos [[set user]::get frame]]]
						}
					}
				}
			} else {
				[set user]::set local_dragon 1
				[set user]::set old_frame [[set user]::get frame]
				[set user]::set frame     [::flds::todrac [[set user]::get frame]]

				set x [::furc::convert::itos [lindex [[set user]::get camera] 0]]
				set y [::furc::convert::itos [lindex [[set user]::get camera] 1]]

				[set fd]::puts C "(\[D\] Draconic illusion enabled!"
				[set fd]::puts S [format {desc %s [Draconic Illusion]} [[set user]::get desc]]
				putlog [format "\[FLDS\] %s : Draconic illusion enabled!" [[set user]::get name]]

				# Displaying to all in range #
				foreach usr [::furc::user::list] {
					if {[[set usr]::get location] == [[set user]::get location]} {
						set ux [lindex [[set user]::get camera] 0]
						set uy [lindex [[set user]::get camera] 1]
						set rx [lindex [[set usr]::get  camera] 0]
						set ry [lindex [[set usr]::get  camera] 1]

						if {(($ux <= [expr $rx + 7]) && ($ux >= [expr $rx - 7])) && (($uy <= [expr $ry + 8]) && ($uy >= [expr $ry - 8]))} {
							[[set usr]::get fd]::puts C [format {]vb%s%s} $x $y]
							[[set usr]::get fd]::puts C [format {<%s%s%s%s} [string range [[set user]::get colormap] 0 9] $x $y [::furc::convert::itos [[set user]::get frame]]]
						}
					}
				}
			}
		}
	} elseif {($cmd == {dragon}) && [::flds::check $user]} {
		::flds::handler::raw $fd ldrac
	} else {
		if {[::flds::check $user]} {
			# Illusion #
			set x    [::furc::convert::itos [lindex [[set user]::get camera] 0]]
			set y    [::furc::convert::itos [lindex [[set user]::get camera] 1]]

			if {$cmd == {breath}} {
				set type {a} ;# Dragon Breath
			} else {
				set type {b} ;# Phoenix Flame
			}

			# Sending to all members in range #
			foreach usr [::furc::user::list] {
				if {[[set usr]::get location] == [[set user]::get location]} {
					set ux [lindex [[set user]::get camera] 0]
					set uy [lindex [[set user]::get camera] 1]
					set rx [lindex [[set usr]::get  camera] 0]
					set ry [lindex [[set usr]::get  camera] 1]

					if {(($ux <= [expr $rx + 7]) && ($ux >= [expr $rx - 7])) && (($uy <= [expr $ry + 8]) && ($uy >= [expr $ry - 8]))} {
						[[set usr]::get fd]::puts C [format {]v%s%s%s} $type $x $y]
					}
				}
			}
		} else {
			return 0
		}
	}

	return 1
}

proc ::flds::handler::msg {fd data} {
	if {[lrange [split $data] 0 5] == {* You are not a dragon.}} {
		# Turning on illusion #
		::flds::handler::raw $fd ldrac
	} else {
		return 0
	}

	return 1
}

proc ::flds::handler::look {fd cmap name} {
	set user [[set fd]::get user]

	# Checking if it's for us #
	if {[set usr [::flds::nameloc $name]] == {}} {
		return 0
	}

	if {![::flds::check $usr]} {
		return 0
	}

	# Applying Illusion #
	set newcmap "[string range $cmap 0 11]5"
	[set fd]::puts C [format {]f%s%s} $newcmap $name]
	return 1
}

proc ::flds::handler::move {fd cmap frame src dst dir} {
	set user [[set fd]::get user]

	# Checking if it's for us #
	set flag 0
	foreach usr [::furc::user::list] {
		if {$dst == [[set usr]::get camera]} {
			incr flag
			break
		}
	}

	if {!$flag} {
		return 0
	}

	if {![::flds::check $usr]} {
		return 0
	}

	# Converting #
	[set usr]::set old_frame $frame

	set newframe [::furc::convert::itos [::flds::todrac $frame]]
	set sx       [::furc::convert::itos [lindex $src 0]]
	set sy       [::furc::convert::itos [lindex $src 1]]
	set dx       [::furc::convert::itos [lindex $dst 0]]
	set dy       [::furc::convert::itos [lindex $dst 1]]

	[set fd]::puts C [format {/%s%s%s%s%s%s} $cmap $dx $dy $newframe $sx $sy]
	return 1
}

proc ::flds::handler::spawn {fd cmap frame coord} {
	set user [[set fd]::get user]

	# Checking if it's for us #
	set flag 0
	foreach usr [::furc::user::list] {
		if {$coord == [[set usr]::get camera]} {
			incr flag
			break
		}
	}

	if {!$flag} {
		return 0
	}

	if {![::flds::check $usr]} {
		return 0
	}

	# Conversion #
	[set usr]::set old_frame $frame

	set newframe [::furc::convert::itos [::flds::todrac $frame]]
	set x        [::furc::convert::itos [lindex $coord 0]]
	set y        [::furc::convert::itos [lindex $coord 1]]

	[set fd]::puts C [format {<%s%s%s%s} $cmap $x $y $newframe]
	return 1
}

### INITIALIZATION ###########################################
if {[catch {::furc::version} ver]} {
	putlog "\[FLDS\] Unable to load - FURC module was not found!"
} else {
	scan $ver {%d.%d.%d} major minor bugfix

	if {($major < 1) || ($minor < 1)} {
		putlog "\[FLDS\] Unable to load - version inconsistency ($ver - needed 1.1.0+)"
		::flds::cleanup
	} else {
		::furc::hook::add C RAW   ::flds::handler::raw
		::furc::hook::add S MSG   ::flds::handler::msg
		::furc::hook::add S LOOK  ::flds::handler::look
		::furc::hook::add S MOVE  ::flds::handler::move
		::furc::hook::add S SPAWN ::flds::handler::spawn
	}
}

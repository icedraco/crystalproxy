##[1.0.0]################[http://icerealm.loktuslumina.com]##
# furc_whisp.tcl # Furcadian Whisper Addon                  #
##[icedragon@quickfox.org]###################################

namespace eval ::fwam {
	# Variables #
	variable version {1.0.0} ;# SubModule version number

	# Functions #
	proc version    {} {} ;# SubModule version
	proc cleanup    {} {} ;# SubModule disposal
	proc getlast    {} {} ;# Get the last whisper nick
	proc hand:msg   {} {} ;# SYstem message handler
	proc hand:whisp {} {} ;# Whisper handler
}

#############################################################

proc ::fwam::version {} {
	set [namespace current]::version
}

proc ::fwam::cleanup {} {
	# Unlinking hooks #
	::furc::hook::del S ::fwam::hand:msg
	::furc::hook::del C ::fwam::hand:whisper

	# Removing variables #
	foreach user [::furc::user::list] {
		catch {
			[set user]::unset last_whisper
		}
	}

	# Removing namespace #
	namespace delete ::fwam
}

proc ::fwam::getlast {user} {
	if {![catch {[set user]::get last_whisper} name]} {
		return $name
	}

	return
}

proc ::fwam::hand:msg {fd data} {
	# Is it for us? #
	if {![regexp {\[ .+ whispers, ".+" to you. \]} $data]} {
		return 0
	}

	set user [[set fd]::get user]
	set src  [lindex [split $data] 1]

	# Storing the last nick #
	[set user]::set last_whisper $src
	return 0
}

proc ::fwam::hand:whisper {fd target data} {
	set user [[set fd]::get user]

	# Is it for us? #
	if {[string index $target 0] != {/}} {
		[set user]::set last_whisper $target
		return 0
	}

	# Getting last nick #
	set name [getlast $user]

	if {$name == {}} {
		[set fd]::puts C {(#BH Last name was not recorded! Please specify a destination.}
		return 1
	}

	# Resorting data #
	if {$data != {}} {
		set data [format { %s} $data]
	}

	set message [format {%s%s} [string range $target 1 end] $data]
	set target   $name

	[set fd]::puts S [format {wh %s %s} $target $message]
	return 1
}

#############################################################

if {[catch {::furc::version} ver]} {
	putlog "\[FNOT\] Unable to load - FURC module was not found!"
} else {
	scan $ver {%d.%d.%d} major minor bugfix

	if {($major < 1) || ($minor < 1)} {
		putlog "\[FNOT\] Unable to load - version inconsistency ($ver - needed 1.1.0+)"
		::fwam::cleanup
	} else {
		::furc::hook::add S MSG     ::fwam::hand:msg
		::furc::hook::add C WHISPER ::fwam::hand:whisper
	}
}

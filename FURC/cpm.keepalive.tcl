##[1.1]###############################[icedragon@quickfox.org]##
# cpm.keepalive.tcl # Keepalive module for CrystalProxy2 users #
################################################################
# This module will transmit an "impulse" every set ammount of  #
# time to prevent the server from dropping the users due to a  #
# lack of activity.                                            #
################################################################

namespace eval ::cpka {
	# Variables #
	variable fuse      1        ;# Safety lock
	variable interval  60       ;# Interval between impulses (seconds)
	variable string   {iamhere} ;# Impulse string

	# Functions #
	proc impulse {} {}          ;# Keepalive impulse generator
	proc ktimer  {} {}          ;# Purge the timers
	proc cleanup {} {}          ;# Module cleanup procedure
}


###--# impulse #--###
proc ::cpka::impulse {} {
	# Transmitting impulse #
	foreach fd [::furc::fd::list] {
		catch {
			[set fd]::puts S $::cpka::string
		}
	}

	# Purging old timers #
	ktimer

	# Restarting the timer #
	utimer $::cpka::interval ::cpka::impulse
}


###--# cleanup #--###
proc ::cpka::ktimer {} {
	foreach timer [utimers] {
		if {[lindex $timer 1] == {::cpka::impulse}} {
			killutimer [lindex $timer 2]
		}
	}
}


proc ::cpka::cleanup {} {
	# Purging timer #
	ktimer

	# Removing the namespace #
	namespace delete ::cpka
}

################################################################
if {![info exists ::furc::version]} {
	::cpka::cleanup
	error {CPKA Error - CrystalProxy was not found!}
} else {
	scan $::furc::version {%d.%d.%d} major minor bug
	if {($major < 2) || ($minor < 1)} {
		::cpka::cleanup
		error [format {CPKA Error - CrystalProxy version inconsistency: %s (Needed 2.1.0+)} $::furc::version]
	} else {
		if {$::cpka::fuse} {
			::furc::hook::add 3 C TERM ::cpka::cleanup
			::cpka::impulse
		}
	}
}
### END OF MODULE ##############################################

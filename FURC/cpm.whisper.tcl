##[1.1]########################################[icedragon@quickfox.org]##
# cpm.whisper.tcl # Whisper modification module for CrystalProxy2 users #
#########################################################################
# This module allows CrystalProxy members to whisper to each other      #
# without involving the server in the process (whisper optimization)    #
# along with some other features it provides.                           #
# DataBase module v1.1 or higher is required!                           #
#########################################################################

namespace eval ::cpwm {
	# Variables #
	variable fuse      0 ;# Safety lock
	variable me        1 ;# /me -> : converter

	# Functions #
	proc cleanup {} {}   ;# Module cleanup procedure

	# Traps #
	namespace eval trap {}
}

###--# cleanup #--###
proc ::cpwm::cleanup {} {
	# Unlinking #
	::furc::hook::del S ::cpwm::trap::whisp:in
	::furc::hook::del C ::cpwm::trap::whisp:out
	::furc::hook::del C ::cpwm::cleanup

	# Purging the namespace #
	namespace delete ::cpwm
}

### TRAPS ###############################################################
namespace eval ::cpwm::trap {
	# Traps #
	proc whisp:in  {} {} ;# Server->Client whisper trap
	proc whisp:out {} {} ;# Client->Server whisper trap
}

###--# whisp:in #--###
proc ::cpwm::trap::whisp:in {fd name data} {
	# Nothing here anymore #
	return 0
}


###--# whisp:out #--###
proc ::cpwm::trap::whisp:out {fd name data} {
	# /me converter #
	if {$::cpwm::me} {
		if {[string tolower $name] == {me}} {
			[set fd]::puts S [format {:%s} $data]
			return 3
		}
	}

	# Optimization #
	set user [::cpdb::find $name]

	if {$user != {}} {
		# Checking for action #
		if {[string index $data 0] == {:}} {
			set data [format {%s %s} [[[set fd]::get user]::get name] [string range $data 1 end]]
		}

		# Specitag #
		[[set user]::get fd]::puts C [format {]-#A%s} [[[set fd]::get user]::get cmap]]

		# Whisper hook #
		set ret [::furc::hook::do S WHISP [list [[set user]::get fd] [[[set fd]::get user]::get name] $data]]

		# Will it block default data? #
		if {![regexp {(2|3)} $ret]} {
			[[set user]::get fd]::puts C [format {([ %s whispers, "%s" to you. ]} [[[set fd]::get user]::get name] $data]
		}

		set ret [::furc::hook::do S SYSMSG [list $fd [format {[ You whisper "%s" to %s. ]} $data $name]]]
		if {![regexp {(2|3)} $ret]} {
			[set fd]::puts C [format {([ You whisper "%s" to %s. ]} $data $name]
		}

		return 2
	}

	return 0
}

### INITIALIZATION ######################################################
if {![info exists ::furc::version]} {
	::cpwm::cleanup
	error {CPWM Error - CrystalProxy was not found!}
} elseif {![info exists ::cpdb::version]} {
	::cpwm::cleanup
	error {CPWM Error - DataBase module was not found!}
} else {
	scan [set ::furc::version] {%d.%d.%d} major minor bug
	if {($major < 2) || ($minor < 1)} {
		::cpwm::cleanup
		error [format {CPWM Error - CrystalProxy version inconsistency: %s (Needed 2.1.0+)} [set ::furc::version]]
	} else {
		# Safety lock #
		if {$::cpwm::fuse} {
			::furc::hook::add 3 S WHISP ::cpwm::trap::whisp:in
			::furc::hook::add 3 C WHISP ::cpwm::trap::whisp:out
			::furc::hook::add 1 C TERM  ::cpwm::cleanup
		}
	}
}
### END OF MODULE #############################################

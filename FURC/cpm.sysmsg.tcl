##[1.1]##################[icedragon@quickfox.org]##
# cpm.sysmsg.tcl # System Message Transfer Module #
###################################################
# This module was designed to keep track of some  #
# system messages, whispers and examination data  #
# through the terminal.                           #
###################################################

namespace eval ::cpsm {
	# Variables #
	variable fuse    {1}   ;# Safety lock
	variable version {1.1} ;# Module version

	# Functions #
	proc trap:msg    {} {} ;# SysMessage Trap
	proc trap:whisp  {} {} ;# Whisper Trap
	proc trap:look   {} {} ;# Look Trap
	proc trap:raw    {} {} ;# Other stuff
	proc cleanup     {} {} ;# Module cleanup
}


###--# trap:msg #--###
proc ::cpsm::trap:msg {fd data} {
	putlog [format {[SMSG] %s: %s} [namespace tail $fd] $data]

	if {([lindex [split $data] 1] == {plays}) && ([string index [lindex [split $data] 2] end] == {.}) && [string is digit [string range [lindex [split $data] 2] 0 [expr [string length [lindex [split $data] 2]] - 2]]]} {
	    return 3;
	}

	return 0 ;# Continue
}


###--# trap:whisp #--###
proc ::cpsm::trap:whisp {fd name data} {
	putlog [format {[WHSP] %s: [%s] %s} [namespace tail $fd] $name $data]
	return 0 ;# Continue
}


###--# trap:look #--###
proc ::cpsm::trap:look {fd cmap name} {
	putlog [format {[LOOK] %s: %s - %s} [namespace tail $fd] $cmap $name]
	return 0 ;# Continue
}


###--# trap:raw #--###
proc ::cpsm::trap:raw {fd data} {
	# Reverting The Converted Desctags #

	# Update Filter #
	if {[string range $data 0 1] == {]x}} {
		${fd}::puts C {([X] Update instruction overriden!}
		return 3
	}

	# Visual Effect Filter #
	if {[string range $data 0 1] == {]v}} { return 3 }

	# DS Filter #
#	if {[regexp {6|7|8} [string index $data 0]]} { return 3 }

	if {[string range $data 0 1] == {]s}} {
		set x [string range $data 2 3]
		set y [string range $data 4 5]
		set n [string range $data 6 end]

		putlog [format {[DRPO] %s: Dream portal - [%02d %02d][%s%s] - %s} [namespace tail $fd] [expr [::furc::convert::stoi $x] * 2] [::furc::convert::stoi $y] $x $y $n]
	} elseif {[string index $data 0] == {>}} {
		for {set i 1} {$i < [string length $data]} {incr i 6} {
			set x [string range $data $i            [expr $i + 1]]
			set y [string range $data [expr $i + 2] [expr $i + 3]]
			set f [::furc::convert::stoi [string range $data [expr $i + 4] [expr $i + 5]]]

			if {($f >= 1) && ($f <= 32) && ($f != 3) && ($f != 4)} {
				set addon { [Pillow!]}
			} elseif {$f == 612} {
				set addon { [Cape!] }
			} elseif {$f == 160} {
				set addon { [PEgg!] }
			} else {
				set addon {}
			}

			if {$addon != {}} {
				putlog [format {[SOBJ] %d: Object spawn - %02d%s - [%02d %02d][%s%s]} [namespace tail $fd] $f $addon [expr [::furc::convert::stoi $x] * 2] [::furc::convert::stoi $y] $x $y]
			}
		}
	}
	return 0
}

proc ::cpsm::trap:cmd {fd data} {
    set cmd [lindex [split $data] 0]
    set dat [string range $data [expr [string first { } $data] + 1] end]

    if {([namespace tail $fd] == 0) && (($cmd == {<}) || ($cmd == {>}))} {
	if {$cmd == {<}} {
	    incr ::cpsm::temp_port {-1}
	    ${fd}::puts C [format {([<] Portrait #%d} $::cpsm::temp_port]
	} else {
	    incr ::cpsm::temp_port
	    ${fd}::puts C [format {([>] Portrait #%d} $::cpsm::temp_port]
	}
	::cpsm::trap:cmd $fd [format {v%s} $::cpsm::temp_port]
    } elseif {$cmd == {echo}} {
	${fd}::puts C $dat
    } elseif {$cmd == {bcast}} {
	::furc::fd::broadcast C $dat
    } elseif {($cmd == {viewport}) || (([string index $cmd 0] == {v}) && ([string is digit [string index $cmd 1]]))} {
	if {[string is digit [string index $cmd 1]]} {
	    set dat [string range $cmd 1 end]
	    if {[namespace tail $fd] == 3} {
		set ::cpsm::temp_port $dat
	    }
	} else {
	    set dat [lindex [split $dat] 0]
	}
	if {![string is digit $dat]} {
	    ${fd}::puts C {([P] Syntax: `viewport <positive number>}
	    return 3
	}

	${fd}::puts C [format {]&%d pp%d} $dat $dat]
    } else {
	return 0
    }

    return 3
}


###--# cleanup #--###
proc ::cpsm::cleanup {} {
	# Unlinking hooks #
	::furc::hook::del S ::cpsm::trap:msg
	::furc::hook::del S ::cpsm::trap:whisp
	::furc::hook::del S ::cpsm::trap:look
	::furc::hook::del S ::cpsm::trap:raw
	::furc::hook::del C ::cpsm::trap:cmd

	# Purging the namespace #
	namespace delete ::cpsm
}


###################################################
if {![info exists ::furc::version]} {
	::cpsm::cleanup
	error {CPSM Error - CrystalProxy was not found!}
} else {
	scan [set ::furc::version] {%d.%d.%d} major minor bug
	if {($major < 2) || ($minor < 1)} {
		::cpsm::cleanup
		error [format {CPSM Error - CrystalProxy version inconsistency: %s (Needed 2.1.0+)} [set ::furc::version]]
	} else {
		# Safety lock #
		if {$::cpsm::fuse} {
			::furc::hook::add 3 S TERM   ::cpsm::cleanup
			::furc::hook::add 3 S SYSMSG ::cpsm::trap:msg
			::furc::hook::add 3 S WHISP  ::cpsm::trap:whisp
			::furc::hook::add 3 S LOOK   ::cpsm::trap:look
			::furc::hook::add 3 S RAW    ::cpsm::trap:raw
			::furc::hook::add 3 C RAW    ::cpsm::trap:cmd
		}
	}
}
### END OF MODULE #################################

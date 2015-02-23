##[1.0]############[http://icerealm.loktuslumina.com]##
# furc_cmd.tcl # Furcadian Protocol Command Module    #
##[icedragon@quickfox.org]#############################

namespace eval ::fcmd {
	# Variables #
	variable version {1.0} ;# Module version

	# Functions #
	proc breath   {} {} ;# Dragon breath
	proc camera   {} {} ;# Camera movement/positioning
	proc clear    {} {} ;# Coordinate clear
	proc flame    {} {} ;# Phoenix flame
	proc look     {} {} ;# Portrait setting
	proc message  {} {} ;# System message
	proc move     {} {} ;# Movement
	proc spawn    {} {} ;# Spawn/Setting
	proc specitag {} {} ;# Specitag display

	proc getcmd   {} {} ;# Get a transmittion command
	proc cleanup  {} {} ;# Dispose of all FURC objects
	proc version  {} {} ;# Returns the version number
}

### FUNCTIONS #########################################

proc ::fcmd::breath {fd coord} {
	set cmd [getcmd $fd]

	# Coordinates #
	set x [::furc::convert::itos [lindex $coord 0]]
	set y [::furc::convert::itos [lindex $coord 1]]

	# Execution #
	[set cmd] C [format {]va%s%s} $x $y]
}

proc ::fcmd::camera {fd dst {src {-1 -1}}} {
	set cmd [getcmd $fd]

	# Coordinates #
	if {[lindex $src 0] < 0} {
		set sx {}
		set sy {}
	} else {
		set sx [::furc::convert::itos [lindex $src 0]]
		set sy [::furc::convert::itos [lindex $src 1]]
	}

	set dx [::furc::convert::itos [lindex $dst 0]]
	set dy [::furc::convert::itos [lindex $dst 1]]

	# Execution #
	[set cmd] C [format {@%s%s%s%s} $dx $dx $sx $sy]
}

proc ::fcmd::clear {fd coord} {
	set cmd [getcmd $fd]

	# Coordinates #
	set x [::furc::convert::itos [lindex $coord 0]]
	set y [::furc::convert::itos [lindex $coord 1]]

	# Execution #
	[set cmd] C [format {)%s%s} $x $y]
}

proc ::fcmd::flame {fd coord} {
	set cmd [getcmd $fd]

	# Coordinates #
	set x [::furc::convert::itos [lindex $coord 0]]
	set y [::furc::convert::itos [lindex $coord 1]]

	# Execution #
	[set cmd] C [format {]vb%s%s} $x $y]
}

proc ::fcmd::look {fd cmap name} {
	set cmd [getcmd $fd]

	# Execution #
	[set cmd] C [format {]f%s%s} $cmap $name]
}

proc ::fcmd::message {fd message} {
	set cmd [getcmd $fd]

	# Execution #
	[set cmd] C [format {(%s} $message]
}

proc ::fcmd::move {fd cmap dst frame src} {
	set cmd [getcmd $fd]

	# Frame #
	set frame [::furc::convert::itos $frame]

	# Coordinates #
	set dx [::furc::convert::itos [lindex $dst 0]]
	set dy [::furc::convert::itos [lindex $dst 1]]
	set sx [::furc::convert::itos [lindex $src 0]]
	set sy [::furc::convert::itos [lindex $src 1]]

	# Execution #
	[set cmd] C [format {/%s%s%s%s} $cmap $dx $dy $frame $sx $sy]
}

proc ::fcmd::spawn {fd cmap coord frame} {
	set cmd [getcmd $fd]

	# Frame #
	set frame [::furc::convert::itos $frame]

	# Coordinates #
	set x [::furc::convert::itos [lindex $coord 0]]
	set y [::furc::convert::itos [lindex $coord 1]]

	# Execution #
	[set cmd] C [format {<%s%s%s%s} $cmap $x $y $frame]
}

proc ::fcmd::specitag {fd cmap} {
	set cmd [getcmd $fd]

	# Execution #
	[set cmd] C [format {]-#A%s} $cmap]
}


proc ::fcmd::getcmd {fd} {
	# Command setting #
	if {[string tolower [string index $fd 0]] == {a}} {
		# Broadcast #
		return ::furc::fd::broadcast
	} else {
		# Send #
		return [set fd]::puts
	}
}

proc ::fcmd::cleanup {} {
	# Erasing namespace #
	namespace delete ::fcmd
}

proc ::fcmd::version {} {
	set [namespace current]::version
}

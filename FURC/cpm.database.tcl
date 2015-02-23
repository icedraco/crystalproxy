##[1.1]##############################[icedragon@quickfox.org]##
# cpm.database.tcl # CrystalProxy DataBase Maintenance Module #
###############################################################
# This module was designed to collect and maintain various    #
# information about every connected user in order to provide  #
# a more convenient tracking capability and provide a wider   #
# range of possibilities to the moule developers and their    #
# modules.                                                    #
###############################################################

namespace eval ::cpdb {
	# Variables #
	variable fuse    {1}   ;# Setting to 0 locks the linking
	variable version {1.1} ;# Version number

	# Functions #
	proc new     {} {}     ;# Create an entry
	proc del     {} {}     ;# Remove an entry
	proc exists  {} {}     ;# Existence check
	proc list    {} {}     ;# List all entries
#	proc find    {} {}      # Find a specific entry by name
	proc cleanup {} {}     ;# Dispose of the module residents
	proc version {} {}     ;# Return the version
	# Traps #
	namespace eval trap {} ;# Trap handlers
}


###--# new #--###
proc ::cpdb::new {fd} {
	# Determining the number #
	if {![string is digit $fd]} {
		set fd [namespace tail $fd]
	}

	# Are we trying to overwrite? #
	if {[exists "::cpdb::$fd"]} {
		del "::cpdb::$fd"
	}

	# Does the FD exist? #
	if {![::furc::fd::exists "::furc::fd::$fd"]} {
		return 0 ;# Won't be able to maintain it.
	}

	# Creating an entry #
	namespace eval ::cpdb::$fd {
		# Variables #
		variable name    {(Unknown)}     ;# User name
		variable pass    {}              ;# Password
		variable cmap    {            !} ;# Colormap acquired
		variable desc    {}              ;# Character description
		variable coord   {0 0}           ;# Current coordinates
		variable camera  {0 0}           ;# Camera coordinates
		variable frame   {0}             ;# Current frame number
		variable map     {}              ;# Last global map known
		variable patch   {default}       ;# Last patch data known
		variable version {(Unknown)}     ;# Client version
		variable winver  {(Unknown)}     ;# Windows version returned
		variable ctime   {}              ;# Connection timestamp
		variable fd      {}              ;# Assigned descriptor

		# Functions #
		proc del    {} {} ;# Delete the current entry
		proc set    {} {} ;# Set a local variable
		proc get    {} {} ;# Get data from a local variable
		proc unset  {} {} ;# Unset a local variable
		proc exists {} {} ;# Existence check
	}

	# Building functions #

	###--# del #--###
	proc ::cpdb::[::set fd]::del {} {
		namespace delete [namespace current]
	}

	###--# set #--###
	proc ::cpdb::[::set fd]::set {var value} {
		::set [namespace current]::$var $value
	}

	###--# get #--###
	proc ::cpdb::[::set fd]::get {var} {
		::set [namespace current]::$var
	}

	###--# unset #--###
	proc ::cpdb::[::set fd]::unset {var} {
		::unset [namespace current]::$var
	}

	###--# exists #--###
	proc ::cpdb::[::set fd]::exists {} {
		return 1 ;# Duh
	}

	return ::cpdb::$fd
}


###--# del #--###
proc ::cpdb::del {entry} {
	if {![exists $entry]} { return 0 }
	namespace delete $entry
}


###--# exists #--###
proc ::cpdb::exists {entry} {
	namespace exists $entry
}


###--# list #--###
proc ::cpdb::list {} {
	set entries {}

	# Separating entries from normal functions #
	foreach entry [namespace children ::cpdb] {
		if {[string is digit [namespace tail $entry]]} {
			lappend entries $entry
		}
	}

	return $entries
}


###--# find #--###
proc ::cpdb::find {name} {
	foreach user [list] {
		if {[string tolower $name] == [string tolower [[set user]::get name]]} {
			return $user
		}
	}

	return
}


###--# cleanup #--###
proc ::cpdb::cleanup {} {
	# Unlinking #
	::cpdb::trap::unlink

	# Purging the namespace #
	namespace delete ::cpdb
}


###--# version #--###
proc ::cpdb::version {} {
	set [namespace current]::version
}


###############################################################
namespace eval ::cpdb::trap {
	# Traps #
	proc conn   {} {} ;# Client connection trap
	proc auth   {} {} ;# Authentication pass trap
	proc disc   {} {} ;# Disconnection trap

	proc desc   {} {} ;# Description change trap
	proc map    {} {} ;# Mapname change trap
	proc patch  {} {} ;# Patch change trap

	proc camera {} {} ;# Camera movement trap
	proc spawn  {} {} ;# Spawn/Movement trap

	proc raw    {} {} ;# Raw data trap

	# Functions #
	proc link   {} {} ;# Link the traps with the system
	proc unlink {} {} ;# Unlink the traps from the system
	proc lcmd   {} {} ;# Helper function for [link]
}


###--# conn #-###
proc ::cpdb::trap::conn {fd} {
	# Generate a new user entry #
	          set user [::cpdb::new $fd]
	[set fd]::set user  $user

	# Initialization #
	[set user]::set ctime [clock seconds]
	[set user]::set fd     $fd

	return 0
}


###--# auth #--###
proc ::cpdb::trap::auth {fd} {
	# Announcement #
	::furc::fd::broadcast C [format {([X] %s has connected!} [[[set fd]::get user]::get name]]

	# User listing
	[set fd]::puts C {([X] Currently connected users:}
	foreach user [::cpdb::list] {
		if {[[[set user]::get fd]::get state] == {LINK}} {
			[set fd]::puts C [format {([X]  # %s} [[set user]::get name]]
		}
	}

	[set fd]::puts C {([X] End of List}
	return 0
}


###--# disc #--###
proc ::cpdb::trap::disc:a {name} {
	if {$name != {(Unknown)}} {
		::furc::fd::broadcast C [format {([X] %s has disconnected!} $name]
	}
}

proc ::cpdb::trap::disc {fd} {
	# Purging his user entry #
	set name [[[set fd]::get user]::get name]

	[[set fd]::get user]::del
	[set fd]::set  user {}

	# Delayed announcement (otherwise it screws up *shrugs*) #
	utimer 1 "::cpdb::trap::disc:a $name"

	return 0
}


###--# desc #--###
proc ::cpdb::trap::desc {fd desc} {
	# Updating description #
	            set user [[set fd]::get user]
	[set user]::set desc $desc
	return 0
}


###--# map #--###
proc ::cpdb::trap::map {fd mapname} {
	# Updating map name #
	            set user [[set fd]::get user]
	[set user]::set map  $mapname
	return 0
}


###--# patch #--###
proc ::cpdb::trap::patch {fd name id} {
	# Updating patch name #
	            set user  [[set fd]::get user]
	[set user]::set patch $name
	return 0
}


###--# camera #--###
proc ::cpdb::trap::camera {fd dst src} {
	# Updating camera position #
	            set user   [[set fd]::get user]
	[set user]::set camera $dst

	# Is it the first update? #
	if {[[set user]::get coord] == {0 0}} {
		[set user]::set coord $dst
	}

	return 0
}


###--# spawn #--###
proc ::cpdb::trap::spawn {fd cmap coord frame {src $dst}} {
	set user [[set fd]::get user]

	

	# Is it me? #
	if {$coord == [[set user]::get camera]} {
		# Updating information #
		[set user]::set coord $coord
		[set user]::set frame $frame
	}

	return 0
}


###--# raw #--###
proc ::cpdb::trap::raw {fd data} {
	set user [[set fd]::get user]

	# State-independant stuff #
	if {[lindex [split $data] 0] == {Winver}} {
		# Setting windows version #
		set winver [lindex [split $data] 1]
		[set user]::set winver $winver
	} elseif {[lindex [split $data] 0] == {version}} {
		# Setting client version #
		set version [lindex [split $data] 1]
		[set user]::set version $version
	}

	# Client-side RAW data #
	if {[[set fd]::get state] == {AUTH}} {
		if {[lindex [split $data] 0] == {connect}} {
			# Setting username #
			set name [lindex [split $data] 1]
			set pass [lindex [split $data] 2]
			[set user]::set name $name
			[set user]::set pass $pass

			# Loggin machine hash #
			putlog [format {[HASH] [C] %s -> %s} $name [lrange [split $data] 3 end]]
		} elseif {[lindex [split $data] 0] == {color}} {
			# Setting colormap #
			set cmap [string range $data 6 18]
			[set user]::set cmap $cmap
		}
	}

	# Server-side RAW data #
	if {[string index $data 0] == "\\"} {
		putlog [format {[HASH] [S] %s -> %s} [[set user]::get name] [string range $data 1 end]]
	}
	return 0
}


###--# link #--###
proc ::cpdb::trap::link {{func {link}}} {
	# Are we linking or unlinking? #
	if {$func == {link}} {
		set cmd {add}
	} else {
		set cmd {del}
	}

	# Process #
	lcmd $cmd 1 C {CONN   ::cpdb::trap::conn}
	lcmd $cmd 1 C {DISC   ::cpdb::trap::disc}
	lcmd $cmd 1 C {DESC   ::cpdb::trap::desc}
	lcmd $cmd 1 C {RAW    ::cpdb::trap::raw}
	lcmd $cmd 1 S {AUTH   ::cpdb::trap::auth}
	lcmd $cmd 1 S {DISC   ::cpdb::trap::disc}
	lcmd $cmd 1 S {MAP    ::cpdb::trap::map}
	lcmd $cmd 1 S {PATCH  ::cpdb::trap::patch}
	lcmd $cmd 1 S {CAMERA ::cpdb::trap::camera}
	lcmd $cmd 1 S {MOVE   ::cpdb::trap::spawn}
	lcmd $cmd 1 S {SPAWN  ::cpdb::trap::spawn}
	lcmd $cmd 1 S {RAW    ::cpdb::trap::raw}
	lcmd $cmd 3 S {TERM   ::cpdb::cleanup}
}


###--# unlink #--###
proc ::cpdb::trap::unlink {} {
	link purge ;# Nifty, huh?
}


###--# lcmd #--###
proc ::cpdb::trap::lcmd {action pri side hook} {
	if {$action == {add}} {
		::furc::hook::add $pri $side $hook
	} else {
		::furc::hook::del $side $hook
	}
}


###############################################################
if {![info exists ::furc::version]} {
	::cpdb::cleanup
	error {CPDB Error - CrystalProxy was not found!}
} else {
	scan [set ::furc::version] {%d.%d.%d} major minor bug
	if {($major < 2) || ($minor < 1)} {
		::cpdb::cleanup
		error [format {CPDB Error - CrystalProxy version inconsistency: %s (Needed 2.1.0+)} [set ::furc::version]]
	} else {
		# Safety lock - allows us to disable the updates #
		if {$::cpdb::fuse} {
			::cpdb::trap::link
		}
	}
}
### END OF MODULE #################################

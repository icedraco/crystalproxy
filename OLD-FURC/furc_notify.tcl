##[1.0.0]################[http://icerealm.loktuslumina.com]##
# furc_notify.tcl # Furcadian Online Notification SubModule #
##[icedragon@quickfox.org]###################################

namespace eval ::fnot {
	# Variables #
	variable interval {60}    ;# Interval between nicks
	variable maximum  {10}    ;# Maximal ammount of nicks in the list
	variable version  {1.0.0} ;# Submodule version

	# Functions #
	proc handle  {} {} ;# Notify event handler
	proc rawhand {} {} ;# RAW handler (for commands)
	proc poll    {} {} ;# Poll function
	proc request {} {} ;# Request online information

	proc add     {} {} ;# Add a name to the list
	proc del     {} {} ;# Remove a nema from the list
	proc find    {} {} ;# Find a name in the list
	proc list    {} {} ;# Find a name in the list

	proc version {} {} ;# Returns the subscript version
	proc cleanup {} {} ;# Removes subscript residents
}

### FUNCTIONS ###############################################
proc ::fnot::handle {fd name state} {
	foreach user [::furc::user::list] {
		set new_list {}
		foreach entry [list $user] {
			set e_name  [lindex $entry 0]
			set e_state [lindex $entry 1]

			if {([string tolower $e_name] == [string tolower $name]) && ($e_state != $state)} {
				if {$state} {
					set vstate "online"
				} else {
					set vstate "offline"
				}
				[[set user]::get fd]::puts C [format {([N] %s is %s!} $e_name $vstate]
				putlog [format {[FNOT] %s : %s is %s!} [[[set fd]::get user]::get name] $e_name $vstate]
				lappend new_list [::list $name $state]
			} else {
				lappend new_list [::list $e_name $e_state]
			}
		}
		[set user]::set notify_list $new_list
	}

	return 0
}

proc ::fnot::rawhand {fd data} {
	# Checking if we're needed #
	if {[string tolower [lindex [split $data] 0]] != "notify"} {
		return 0 ;# Not us...
	}

	# Locating sub-commands #
	set cmd [string tolower [lindex [split $data] 1]]

	if {$cmd == {}} {
		set cmd {list} ;# Default command
	}

	set user [[set fd]::get user]

	# Handling sub-commands #
	switch $cmd {
		{help} {
			set buffer {}
			append buffer "(\[N\] === NOTIFY HELP ==========\n"
			append buffer "(\[N\] help        - This help screen\n"
			append buffer "(\[N\] list         - Lists all the users\n"
			append buffer "(\[N\] add <name> - Adds a name\n"
			append buffer "(\[N\] del <name> - Removes a name\n"
			append buffer "(\[N\] ==========================\n"
			[set fd]::puts C $buffer
		}
		{list} {
			set buffer "(\[N\] === NOTIFY LIST ==========\n"
			set nlist  [list $user]
			if {$nlist == {}} {
				[set fd]::puts C "(\[N\] The notify list is empty!"
			} else {
				foreach entry $nlist {
					if {[lindex $entry 1]} {
						set vstate "+"
					} else {
						set vstate "-"
					}
					append buffer [format "(\[N\] \[%s\]%s\n" $vstate [lindex $entry 0]]
				}
				[set fd]::puts C $buffer
			}
		}
		{add} {
			if {[lindex [split $data] 2] == {}} {
				[set fd]::puts C "(\[N\] Name was not specified!"
			} else {
				foreach name [lrange [split $data] 2 end] {
					add $user $name
				}
			}
		}
		{del} {
			if {[lindex [split $data] 2] == {}} {
				[set fd]::puts C "(\[N\] Name was not specified!"
			} else {
				foreach name [lrange [split $data] 2 end] {
					del $user $name
				}
			}
		}
		default {
			[set fd]::puts C [format {([N] Unknown sub-command '%s'.} $cmd]
		}
	}

	return 1
}

proc ::fnot::poll {} {
	# Polling all the server #
	foreach user [::furc::user::list] {
		if {![info exists [set user]::notify_list] && ![catch {::sdms::version} smds_ver]} {
			# Trying to sync with SDMS to get the notify list #
			if {![catch {::sdms::gget furc:notify [string tolower [[set user]::get name]]} static_list]} {
				# Converting static list to notify list #
				set notify_list {}
				foreach name $static_list {
					lappend notify_list [::list $name 0]
				}

				# Synchronizing lists #
				[set user]::set notify_list $notify_list

				if {$static_list != {}} {
					[[set user]::get fd]::puts C {([N] Notify list synchronized!}
				}
			}
		}

		foreach entry [list $user] {
			request $user [lindex $entry 0]
		}
	}

	# Resetting the timer #
	foreach timer [utimers] {
		set func [lindex $timer 1]
		set tid  [lindex $timer 2]

		if {$func == {::fnot::poll}} {
			killutimer $tid
		}
	}

	if {$::fnot::interval > 0} {
		utimer $::fnot::interval {::fnot::poll}
	}
}

proc ::fnot::request {user name} {
	[[set user]::get fd]::puts S [format {onln %s} $name]
}

proc ::fnot::add {user name} {
	# Already added? #
	if {[find $user $name]} {
		[[set user]::get fd]::puts C [format {([N] The name '%s' is already in the list!} $name]
		return 0
	}

	# Reached maximum? #
	if {[llength [list $user]] >= $::fnot::maximum} {
		[[set user]::get fd]::puts C [format {([N] Reached a maximal name limit (%s)!} $::fnot::maximum]
		return 0
	}

	# Checking if we actually have a notify list #
	if {[catch {[set user]::get notify_list} current_list]} {
		set current_list {} ;# No matter, we'll create one!
	}

	# Adding a name #
	lappend current_list [::list $name 0]
	[set user]::set notify_list $current_list

	# Confirmation/Data update #
	[[set user]::get fd]::puts C [format {([N] Added '%s' to the notify list.} $name]
	request $user $name

	# Synchronizing with SDMS #
	if {![catch {::sdms::version} sdms_version]} {
		set static_list {}
		foreach notify_name $current_list {
			lappend static_list [lindex $notify_name 0]
		}
		::sdms::gset furc:notify [string tolower [[set user]::get name]] $static_list
	}

	return 1
}

proc ::fnot::del {user name} {
	# Search & Destroy! #
	set found 0
	set new_list {}

	# Scanning #
	foreach entry [list $user] {
		if {[string tolower $name] != [string tolower [lindex $entry 0]]} {
			lappend new_list $entry
		} else {
			set found 1
		}
	}

	if {$found} {
		set output [format {([N] The name '%s' has been removed.} $name]
	} else {
		set output [format {([N] The name '%s' was not found!} $name]
	}

	# Syncing the new list #
	[set user]::set notify_list $new_list

	# Syncing with SDMS #
	if {![catch {::sdms::version} sdms_version]} {
		set static_list {}
		foreach notify_name $new_list {
			lappend static_list [lindex $notify_name 0]
		}
		::sdms::gset furc:notify [string tolower [[set user]::get name]] $static_list
	}

	[[set user]::get fd]::puts C $output

	return $found
}

proc ::fnot::find {user name} {
	# Checking if we actually have a notify list #
	if {[catch {[set user]::get notify_list} current_list]} {
		return 0
	}

	foreach entry $current_list {
		if {[string tolower $name] == [string tolower [lindex $entry 0]]} {
			return 1
		}
	}

	return 0
}

proc ::fnot::list {user} {
	# Checking if we actually have a notify list #
	if {[catch {[set user]::get notify_list} current_list]} {
		return
	}

	return $current_list
}

proc ::fnot::version {} {
	set [namespace current]::version
}

proc ::fnot::cleanup {} {
	# Cleaning Timer #
	foreach timer [utimers] {
		set func [lindex $timer 1]
		set tid  [lindex $timer 2]

		if {$func == {::fnot::poll}} {
			killutimer $tid
		}
	}

	# Unlinking the hooks #
	::furc::hook::del C ::fnot::rawhand
	::furc::hook::del S ::fnot::handle

	# Cleaning the user structures #
	foreach user [::furc::user::list] {
		catch {
			[set user]::unset notify_list
		}
	}

	# Disposing of namespaces #
	namespace delete ::fnot
}

### INITIALIZATION ##########################################

if {[catch {::furc::version} ver]} {
	putlog "\[FNOT\] Unable to load - FURC module was not found!"
} else {
	scan $ver {%d.%d.%d} major minor bugfix

	if {($major < 1) || ($minor < 1)} {
		putlog "\[FNOT\] Unable to load - version inconsistency ($ver - needed 1.1.0+)"
		::fnot::cleanup
	} else {
		::furc::hook::add C RAW    ::fnot::rawhand
		::furc::hook::add S NOTIFY ::fnot::handle

		::fnot::poll
	}
}

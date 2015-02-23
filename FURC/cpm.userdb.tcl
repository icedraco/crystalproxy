##[1.0]#########################[icedragon@quickfox.org]##
# cpm.userdb.tcl # User DataBase Module for CrystalProxy #
##########################################################

namespace eval ::cpud {
	# Variables #
	variable fuse    {1}         ;# Safety lock
	variable version {1.0}       ;# Module version

	variable my_host {localhost} ;# MySQL hostname
	variable my_port {3306}      ;# MySQL port number
	variable my_user {icedragon} ;# MySQL username
	variable my_pass {andromeda} ;# MySQL password
	variable my_db   {FURC}      ;# MySQL database name
	variable my_tab  {UserDB}    ;# MySQL table name
	variable my_hand             ;# MySQL handle

	# Functions #
	proc cmatch  {} {}     ;# Colormap matching

	proc process {} {}     ;# MySQL compatibility processor
	proc version {} {}     ;# Return module version
	proc cleanup {} {}     ;# Dispose of the module

	# Namespaces #
	namespace eval db   {} ;# DataBase submodule
	namespace eval hand {} ;# Handlers
}

###--# cmatch #--###
proc ::cpud::cmatch {cmap1 cmap2} {
	# Speed-match #
	if {![string compare $cmap1 $cmap2]} { return [list [string length $cmap1] [string length $cmap2]] }

	# Comparison #
	set counter 0

	for {set i 0} {$i < [string length $cmap1]} {incr i} {
		# Get the current characters #
		set cmap(1) [string index $cmap1 $i]
		set cmap(2) [string index $cmap2 $i]

		# Compare #
		if {$cmap(1) == $cmap(2)} {
			incr counter
		}
	}

	# Results #
	return [list $counter [string length $cmap1]]
}

###--# process #--###
proc ::cpud::process {data} {
	::set buffer {}

	for {::set i 0} {$i < [string length $data]} {incr i} {
		::set char [string index $data $i]

		if {$char == {'}} {
			append buffer {\'}
		} else {
			append buffer $char
		}
	}

	return $buffer
}

###--# version #--###
proc ::cpud::version {} {
	set ::cpud::version
}

###--# cleanup #--###
proc ::cpud::cleanup {} {
	# Unlinking Hooks #
	::cpud::hand::unlink

	# Purging pending desc variables #
	foreach fd [::furc::fd::list] {
		# The lazy way #
		catch {
			${fd}::unset UDB_LOOKAT
		}
	}

	# Disconnecting from MySQL #
	catch {
		mysqlclose $::cpud::my_hand
	}

	# Purging Functions #
	namespace delete ::cpud
}

### DATABASE MODULE ######################################
namespace eval ::cpud::db {
	# Functions #
	proc add    {} {} ;# Add an entry
	proc update {} {} ;# Update an entry
	proc del    {} {} ;# Delete an entry
	proc find   {} {} ;# Find an entry
}

###--# add #--###
proc ::cpud::db::add {name cmap {desc {}}} {
	# Do we already have one? #
	if {[find $name] != {}} {
		return [update $name $cmap $desc]
	}

	# Formatting #
	set name [::cpud::process $name]
	set cmap [format {%-13s} [::cpud::process $cmap]]
	set desc [::cpud::process $desc]

	# Store and return #
	mysqlexec $::cpud::my_hand [format {INSERT into %s SET `name` = '%s', `cmap` = '%s', `desc` = '%s'} $::cpud::my_tab $name $cmap $desc]
	return [list $name $cmap $desc]
}

###--# update #--###
proc ::cpud::db::update {name cmap {desc {}}} {
	# Formatting #
	set name [::cpud::process $name]
	set cmap [format {%-13s} [::cpud::process $cmap]]
	set desc [::cpud::process $desc]

	# Update and return #
	mysqlexec $::cpud::my_hand [format {UPDATE %s SET `cmap` = '%s', `desc` = '%s' WHERE `name` = '%s'} $::cpud::my_tab $cmap $desc $name]
	return [list $name $cmap $desc]
}

###--# del #--###
proc ::cpud::db::del {name} {
	# Formatting #
	set name [::cpud::process $name]

	# Purge #
	mysqlexec $::cpud::my_hand [format {DELETE FROM %s WHERE `name` = '%s'} $::cpud::my_tab $name]
	return
}

###--# find #--###
proc ::cpud::db::find {by {value {}}} {
	# Was "by" even specified? #
	if {$value == {}} {
		set value $by
		set by    {N}
	} else {
		set by    [string toupper [string index $by 0]]
	}

	# Formating #
	set value [::cpud::process $value]

	# Sanity check #
	switch $by {
		{N}       { set var {name} }
		{C}       { set var {cmap} }
		{default} { return }
	}

	# Searching #
	set ret [mysqlsel $::cpud::my_hand [format {SELECT * FROM %s WHERE `%s` = '%s'} $::cpud::my_tab $var $value]]

	if {$ret == 0} {
		return ;# Not found
	} else {
		mysqlmap $::cpud::my_hand {name cmap desc} {
			return [list $name $cmap $desc]
		}
	}
}

### DATA HANDLING MODULE #################################
namespace eval ::cpud::hand {
	# Handlers #
	proc look   {} {} ;# Look handler
	proc smsg   {} {} ;# System message handler
	proc raw    {} {} ;# Client->Server raw data

	# Functions #
	proc link   {} {} ;# Link handlers
	proc unlink {} {} ;# Unlink handlers
}

###--# look #--###
proc ::cpud::hand::look {fd cmap name} {
	# Match against previous colormap #
	set db [::cpud::db::find $name]
	if {$db != {}} {
		set old_cmap [lindex $db 1]
		if {$old_cmap != $cmap} {
			${fd}::puts C [format {(- CMap Change: [%s] -> [%s]} $old_cmap $cmap]
		}
	}

	# Add/Update information #
	::cpud::db::add $name $cmap

	# Prepare for desc reading for that user #
	${fd}::set UDB_LOOKAT $name

	return 0
}

###--# smsg #--###
proc ::cpud::hand::smsg {fd data} {
	# Is it a description? #
	if {[string index $data 0] != {>}} { return 0 } ;# Apparently not

	# Record it #
	if {![catch {${fd}::get UDB_LOOKAT} name]} {
		${fd}::unset UDB_LOOKAT

		set cmap [lindex [::cpud::db::find $name] 1]
		set desc [string range $data 2 end]

		::cpud::db::update $name $cmap $desc
	}

	return 0
}

###--# raw #--###
proc ::cpud::hand::raw {fd data} {
	set cmd   [lindex [split [string tolower $data]] 0]
	set name  [lindex [split $data] 1]
	set name2 [lindex [split $data] 2]

	# Sanity checks #
	if {![regexp {^(colorlist|colormatch|clookup|dbstatus|lookup)$} $cmd]} { return 0 } ;# Nope

	# Command parsing! Yey! #
	if {$cmd == {colorlist}} { ;# List all names with the same colors
		# Do we have a name to match against? #
		if {$name == {}} {
			${fd}::puts C {([U] Syntax: `colorlist <name>}
			return 2
		}

		# Checking for name existence #
		set db [::cpud::db::find $name]

		if {$db == {}} {
			${fd}::puts C [format {([U] The name %s is not in my database!} $name]
			return 2
		}

		# Listing names #
		set name [lindex $db 0]
		set cmap [lindex $db 1]
		set desc [lindex $db 2]

		if {[mysqlsel $::cpud::my_hand [format {SELECT * FROM %s WHERE `cmap` = '%s'} $::cpud::my_tab $cmap]] > 0} {
			set counter 10 ;# Truncation
			mysqlmap $::cpud::my_hand {name cmap desc} {
				if {$counter <= 0} {
					${fd}::puts C {([U] === LIST TRUNCATED ==========}
					return 2
				} else {
					${fd}::puts C [format {([U] [%s] %s} $cmap $name]
					incr counter -1
				}
			}
		}
	} elseif {$cmd == {colormatch}} {
		# Do we have the first name #
		if {($name == {}) || ($name2 == {})} {
			${fd}::puts C {([U] Syntax: `colormatch <name> <name2>}
			return 2
		} elseif {[string tolower $name] == [string tolower $name2]} {
			${fd}::puts C {([U] Don't be stupid, for it's impossible for one not to match his own self}
			return 2
		}

		# Checking for name existence #
		set db(1) [::cpud::db::find $name]
		set db(2) [::cpud::db::find $name2]

		if {$db(1) == {}} {
			${fd}::puts C [format {([U] The name %s is not in my database!} $name]
			return 2
		} elseif {$db(2) == {}} {
			${fd}::puts C [format {([U] The name %s is not in my database!} $name2]
			return 2
		}

		# Retreiving necessary data #
		for {set i 1} {$i <= 2} {incr i} {
			set n($i) [lindex $db($i) 0]
			set c($i) [format {%-13s} [lindex $db($i) 1]]
		}

		# Matching #
		scan [::cpud::cmatch [string range $c(1) 0 9] [string range $c(2) 0 9]] {%d %*d} color(ok)
		scan [::cpud::cmatch $c(1) $c(2)] {%d %*d} full(ok)

		${fd}::puts C         {([U] === COLOR MATCH ==========}
		${fd}::puts C [format {([U] [%s] %s} $c(1) $n(1)]
		${fd}::puts C [format {([U] [%s] %s} $c(2) $n(2)]
		${fd}::puts C [format {([U] Color match: %02d/%02d [%f%%]} $color(ok) 10 [expr $color(ok) * 100. / 10]]
		${fd}::puts C [format {([U] Full match:  %02d/%02d [%f%%]} $full(ok)  13 [expr $full(ok)  * 100. / 13]]
		${fd}::puts C         {([U] === END OF MATCH =========}
	} elseif {$cmd == {clookup}} {
		set query [string range $data 8 20]

		set result [mysqlsel $::cpud::my_hand [format {SELECT * FROM %s WHERE `cmap` LIKE '%s%%'} $::cpud::my_tab $query]]

		# Did we find something? #
		if {$result < 1} {
			${fd}::puts C [format {([U] No matches found for [%s]} $query]
		} else {
			${fd}::puts C         {([U] === COLOR MATCH ==========}
			set i 0
			mysqlmap $::cpud::my_hand {name cmap desc} {
				incr i
				${fd}::puts C [format {([U] [%-13s] %s} $cmap $name]
				if {$i >= 10} {
					${fd}::puts C [format {([U] Truncated: 10/%d} $result]
					break
				}
			}
			${fd}::puts C {([U] === END OF MATCH =========}
		}
	} elseif {$cmd == {dbstatus}} {
		set qty [mysqlsel $::cpud::my_hand [format {SELECT * FROM %s} $::cpud::my_tab]]

		if {$qty == 1} {
			set word {entry}
		} else {
			set word {entries}
		}

		${fd}::puts C [format {([U] %d %s found in the database.} $qty $word]
	} elseif {$cmd == {lookup}} {
		# Do we have the first name #
		if {$name == {}} {
			${fd}::puts C {([U] Syntax: `lookup <name>}
			return 2
		}

		# Checking for name existence #
		set db [::cpud::db::find $name]

		if {$db == {}} {
			${fd}::puts C [format {([U] The name %s is not in my database!} $name]
			return 2
		}

		set name [lindex $db 0]
		set cmap [lindex $db 1]
		set desc [lindex $db 2]

		# Displaying the entry #
		${fd}::puts C [format {]f%s%s} $cmap $name]
		${fd}::puts C [format {((Displaying cached info for %s.)} $name]
		${fd}::puts C [format {(> %s} $desc]
	}

	return 2
}

###--# link #--###
proc ::cpud::hand::link {} {
	# Linking Hooks #
	::furc::hook::add 3 S LOOK   ::cpud::hand::look
	::furc::hook::add 3 S SYSMSG ::cpud::hand::smsg
	::furc::hook::add 3 C RAW    ::cpud::hand::raw
	::furc::hook::add 1 C TERM   ::cpud::cleanup
}

###--# unlink #--###
proc ::cpud::hand::unlink {} {
	# Unlinking Hooks #
	::furc::hook::del S ::cpud::hand::look
	::furc::hook::del S ::cpud::hand::smsg
	::furc::hook::del C ::cpud::hand::raw
	::furc::hook::del C ::cpud::cleanup
}

###################################################
if {![info exists ::furc::version]} {
	::cpud::cleanup
	error {CPUD Error - CrystalProxy was not found!}
} else {
	scan [set ::furc::version] {%d.%d.%d} major minor bug
	if {($major < 2) || ($minor < 1)} {
		::cpud::cleanup
		error [format {CPUD Error - CrystalProxy version inconsistency: %s (Needed 2.1.0+)} [set ::furc::version]]
	} else {
		# Safety lock #
		if {$::cpud::fuse} {
			# Checking for mysqltcl package #
			if {[catch {package require mysqltcl} pkgver]} {
				::cpud::cleanup
				error [format {CPUD Error - Unable to load mysqltcl: %s} $pkgver]
			}

			# Checking for existing handle #
			if {![info exists ::cpud::my_hand]} {
				if {[catch {mysqlconnect -host $::cpud::my_host -port $::cpud::my_port -user $::cpud::my_user -pass $::cpud::my_pass -db $::cpud::my_db} ::cpud::my_hand]} {
					set msg $::cpud::my_hand
					::cpud::cleanup
					error [format {CPUD Error - MySQL connection failed: %s} $msg]
				}
			} else {
				if {[catch {mysqlping $::cpud::my_hand} ret] || !$ret} {
					catch { mysqlclose $::cpud::my_hand }
					if {[catch {mysqlconnect -host $::cpud::my_host -port $::cpud::my_port -user $::cpud::my_user -pass $::cpud::my_pass -db $::cpud::my_db} ::cpud::my_hand]} {
						set msg $::cpud::my_hand
						::cpud::cleanup
						error [format {CPUD Error - MySQL connection failed: %s} $msg]
					}
				}
			}

			# Checking for table existence #
			if {[catch {mysqlsel $::cpud::my_hand [format {SELECT * FROM %s} $::cpud::my_tab] -list}]} {
				# Creating the table #
				if {[catch {mysqlexec $::cpud::my_hand [format {CREATE TABLE %s (`name` TEXT NOT NULL, `cmap` CHAR(13), `desc` TEXT)} $::cpud::my_tab]} err]} {
					::cpud::cleanup
					error [format {CPUD Error - Unable to create the table '%s': %s} $::cpud::my_tab $err]
				}
			}

			# Linking Hooks #
			::cpud::hand::link
		}
	}
}
### END OF MODULE #################################

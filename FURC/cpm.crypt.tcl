##[1.0]##############################[icedragon@quickfox.org]##
# cpm.crypt.tcl # CrystalProxy Cryptographic Module           #
###############################################################

namespace eval ::cpcm {
	# Variables #
	variable fuse       {1}   ;# Setting to 0 locks the linking
	variable version    {1.0} ;# Module version
	variable nocorrupt  {1}   ;# Should we block corrupted text?

	# Functions #
	proc key            {} {} ;# Set/Get the encryption key
	proc state          {} {} ;# Encryption setting (ACTIVE/INACTIVE)

	proc version        {} {} ;# Return the module version
	proc cleanup        {} {} ;# Dispose of the module
	# Namespaces #
	namespace eval trap {}   ;# Event traps
}


###--# setkey #--###
proc ::cpcm::key {user {keyword {}}} {
	# Are we retreiving the key? #
	if {$keyword == {}} {
		# Does it exist? #
		if {![info exists ${user}::CRYPT_KEY]} {
			${user}::set CRYPT_KEY {}
		}

		${user}::get CRYPT_KEY
	} else {
		${user}::set CRYPT_KEY $keyword
	}
}


###--# getkey #--###
proc ::cpcm::getkey {user} {
	# Does it exist? #

	# Get the key #
	${user}::get CRYPT_KEY
}


###--# setstate #--###
proc ::cpcm::state {user {state {}}} {
	# Are we retreiving the state? #
	if {$state == {}} {
		# Does it exist? #
		if {![info exists ${user}::CRYPT_STATE]} {
			return [state $user 0]
		}

		return [${user}::get CRYPT_STATE]
	}

	# Is the state boolean? #
	if {($state != 0) && ($state != 1)} {
		error [format {::cpsm::setstate [user=%s state=%s] -- The state doesn't seem to be boolean (1/0)!} $user $state]
	}

	# Set the state #
	${user}::set CRYPT_STATE $state
}


###--# version #--###
proc ::cpcm::version {} {
	set ::cpcm::version
}


###--# cleanup #--###
proc ::cpcm::cleanup {} {
	### NOTHING HERE YET ###
}


###############################################################
namespace eval ::cpcm::trap {
	# Traps #
	proc act     {} {} ;# Client-side action trap
	proc say     {} {} ;# Client-side talk trap
	proc shout   {} {} ;# Client-side shout trap
	proc whisp   {} {} ;# Client-side whisper trap
	proc raw     {} {} ;# Client-side raw data trap

	proc swhisp  {} {} ;# Server-side whisper trap
	proc sysmsg  {} {} ;# System message trap

	# Functions #
	proc crypt   {} {} ;# Encryption mutual function
	proc dcrypt  {} {} ;# Decryption mutual function

	proc link    {} {} ;# Link the traps
	proc unlink  {} {} ;# Unlink the traps
	proc lcmd    {} {} ;# Helpter function for [link]
}


###--# act #--###
proc ::cpcm::trap::act {fd data} {
	# Getting the user entry #
	set user [${fd}::get user]

	return [crypt $user {:} $data]
}


###--# say #--###
proc ::cpcm::trap::say {fd data} {
	# Getting the user entry #
	set user [${fd}::get user]

	return [crypt $user {"} $data] ;# Bleeding -.- "
}


###--# shout #--###
proc ::cpcm::trap::shout {fd data} {
	# Do we even send anything? #
	if {$data == {}} { return 0 }

	# Getting the user entry #
	set user [${fd}::get user]

	return [crypt $user {-} $data]
}


###--# whisp #--###
proc ::cpcm::trap::whisp {fd target data} {
	# Getting the user entry #
	set user [${fd}::get user]

	return [crypt $user [format {wh %s } $target] $data]
}


###--# raw #--###
proc ::cpcm::trap::raw {fd data} {
	# Getting the user entry #
	set user [${fd}::get user]
	set cmd  [lindex [split $data] 0]

	# Client-side RAW data #
	if {![regexp {^(crypt|key)$} $cmd]} { return 0 }

	# Parsing commands #
	if {$cmd == {crypt}} {
		set arg [string tolower [lindex [split $data] 1]]

		if {$arg == {}} {
			# Changing the switch in one line! Just watch *grin* #
			set arg [expr abs([::cpcm::state $user] - 1)]
		}

		if {($arg == {on}) || ($arg == 1)} {
			set keyword [::cpcm::key $user]
			if {$keyword == {}} {
				# Nothing to encrypt with #
				${fd}::puts C {([C] No crypto-key specified! Type `key <keyword> to set one, first.}
			} elseif {[::cpcm::state $user]} {
				# Already enabled #
				${fd}::puts C {([C] Cryptography is already enabled!}
			} else {
				# Activation #
				${fd}::puts C [format {([C] Cryptography enabled! [Keyword: %s]} $keyword]
				::cpcm::state $user 1
			}
		} elseif {($arg == {off}) || ($arg == 0)} {
			if {![::cpcm::state $user]} {
				# Already disabled #
				${fd}::puts C {([C] Cryptography is already disabled!}
			} else {
				# Deactivation #
				${fd}::puts C {([C] Cryptography disabled!}
				::cpcm::state $user 0
			}
		} else {
			# Syntax error #
			${fd}::puts C {([C] Syntax: `crypt [on/off]}
		}
	} elseif {$cmd == {key}} {
		set keyword [lindex [split $data] 1]

		if {$keyword == {}} {
			# Display current keyword #
			set keyword [::cpcm::key $user]

			if {$keyword == {}} {
				set keyword {(none)}
			}

			${fd}::puts C [format {([C] Current crypto-key: %s} $keyword]
		} else {
			# Change keyword #
			::cpcm::key $user $keyword

			${fd}::puts C [format {([C] Crypto-key has been set/changed to: %s} $keyword]
		}
	}

	return 3
}


###--# swhisp #--###
proc ::cpcm::trap::swhisp {fd name data} {
	# Getting the user entry #
	set user [${fd}::get user]

	# Checking for encryption prefix #
	if {[string range $data 0 4] == {==CC:}} {
		# Decrypting #
		set data [dcrypt $user [string range $data 5 end]]

		# Displaying in a compatible way #
		set ret [::furc::hook::do S WHISP [list $fd $name $data]]

		# Will it block default data? #
		if {![regexp {(2|3)} $ret]} {
			${fd}::puts C [format {([ %s cr-whispers, "%s" to you. ]} $name $data]
		}

		return 3 ;# We've caused enough damage for one cycle already ;)
	}
}


###--# sysmsg #--###
proc ::cpcm::trap::sysmsg {fd data} {
	# Getting the user entry #
	set user [${fd}::get user]

	# Is it a crypto-whisper confirmation? #
	if {[regexp {^\[ You whisper "==CC:.+" to .+ ]$} $data]} {
		set    crypto [string range $data [expr [string first {==CC:} $data] + 5] [expr [string last {"} $data] - 1]] ;# " bleeding...
		set    buffer [format {[ You cr-whisper "%s} [dcrypt $user $crypto]] ;# " More bleeding!
		append buffer [string range $data [string last {"} $data] end]            ;# " BAH!
	} else {
		set start [string first {==CC:} [lrange [split $data] 0 1]]

		# Checking for encryption prefix in the first word #
		if {$start < 0} {
			# False alarm - proceed further #
			return 0
		}

		set    crypto [string range $data   [expr $start + 5] end]
		set    buffer [format {[cr]|%s} [string range $data 0 [expr $start - 1]]]
		append buffer [dcrypt $user $crypto]
	}

	# Displaying in a compatible way #
	set ret [::furc::hook::do S SYSMSG [list $fd $buffer]]

	# Will it block default data? #
	if {![regexp {(2|3)} $ret]} {
		${fd}::puts C [format {(%s} $buffer]
	}

	return 3 ;# We've caused enough damage for one cycle already ;)
}


###--# crypt #--###
proc ::cpcm::trap::crypt {user prefix data} {
	# Should we encrypt? #
	set keyword [::cpcm::key $user]
	if {[::cpcm::state $user] && ($keyword != {})} {
		set newdata [format {==CC:%s} [encrypt $keyword $data]]

		# Transmitting #
		[${user}::get fd]::puts S [format {%s%s} $prefix $newdata]

		return 3 ;# Nasty altering...
	}

	return 0 ;# Nothing changed - proceed
}


###--# dcrypt #--###
proc ::cpcm::trap::dcrypt {user data} {
	set keyword [::cpcm::key $user]
	set newdata [decrypt $keyword $data]

	# Checking for data corruption #
	foreach chr [split $newdata {}] {
		# Are we in a correct ASCII area? #
		scan $chr {%c} c

		if {($c < 32) || ($c > 126)} {
			# How do we handle the corruption? #
			set prefix {[Corrupted]}

			if {$::cpcm::nocorrupt} {
				return $prefix
			} else {
				return [format {%s %s} $prefix $newdata]
			}
		}
	}

	return $newdata
}


###--# link #--###
proc ::cpcm::trap::link {{func {link}}} {
	# Are we linking or unlinking? #
	if {$func == {link}} {
		set cmd {add}
	} else {
		set cmd {del}
	}

	# Process #
	lcmd $cmd 3 C {act    ::cpcm::trap::act}
	lcmd $cmd 3 C {say    ::cpcm::trap::say}
	lcmd $cmd 3 C {shout  ::cpcm::trap::shout}
	lcmd $cmd 3 C {whisp  ::cpcm::trap::whisp}
	lcmd $cmd 3 C {raw    ::cpcm::trap::raw}
	lcmd $cmd 1 S {WHISP  ::cpcm::trap::swhisp}
	lcmd $cmd 1 S {SYSMSG ::cpcm::trap::sysmsg}
	lcmd $cmd 3 S {TERM   ::cpcm::cleanup}
}


###--# unlink #--###
proc ::cpcm::trap::unlink {} {
	link purge ;# Nifty, huh?
}


###--# lcmd #--###
proc ::cpcm::trap::lcmd {action pri side hook} {
	if {$action == {add}} {
		::furc::hook::add $pri $side $hook
	} else {
		::furc::hook::del $side $hook
	}
}


###############################################################
if {![info exists ::furc::version]} {
	::cpcm::cleanup
	error {CPCM Error - CrystalProxy was not found!}
} elseif {![info exists ::cpdb::version]} {
	::cpcm::cleanup
	error {CPCM Error - DataBase module was not found!}
} else {
	scan [set ::furc::version] {%d.%d.%d} major minor bug
	if {($major < 2) || ($minor < 1)} {
		::cpcm::cleanup
		error [format {CPCM Error - CrystalProxy version inconsistency: %s (Needed 2.1.0+)} [set ::furc::version]]
	} else {
		# Safety lock #
		if {$::cpcm::fuse} {
			::cpcm::trap::link
		}
	}
}
### END OF MODULE #############################################

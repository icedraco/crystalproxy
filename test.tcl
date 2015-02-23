proc mergefd {src dst} {
    ::furc::fd::${src}::set server [::furc::fd::${dst}::get server]
}

proc sendlook {name} {
    ::furc::fd::1::puts S [format {"look %s} $name] ;# "
}

proc masswho {t names {step {0}}} {
    if {$t      < {0}} { return } ;# Can't go further
    if {$names == {}}  {
	utimer $t "putlog {\[MWHO\] End of recursion}"
	return
    }
    if {$step  == {0}} { set step $t }

    set name [lindex $names 0]
    
    if {[string index $name end] == {,}} {
	set name [string range $name 0 [expr [string last {,} $name] - 1]]
    }
    
    if {[::cpud::db::find $name] == {}} {
	utimer $t "sendlook $name"
	incr t $step
    }

    masswho $t [lrange $names 1 end] $step
}

proc closestmatch {name} {
    set db [::cpud::db::find $name]
    if {$db == {}} { return }
    
    set cmap [lindex $db 1]
    set buff {}
    set max  {0}

    mysqlsel $::cpud::my_hand [format {SELECT * FROM %s} $::cpud::my_tab]
    mysqlmap $::cpud::my_hand {n c d} {
	if {[string tolower $n] == [string tolower $name]} { continue }
	set ret [::cpud::cmatch [string range $c 0 9] [string range $cmap 0 9]]
	if {$max < [lindex $ret 0]} {
	    set buff  $n
	    set max  [lindex $ret 0]
	} elseif {$max == [lindex $ret 0]} {
	    lappend buff $n
	}
    }
    
    putlog [format {[CMMF] Found %d user(s) - %d/10 [%d%%]} [llength $buff] $max [expr $max * 10]]

    foreach n $buff {
	putlog [format {[CMMF] %-13s - %s} [lindex [::cpud::db::find $n] 1] $n]
    }
    
    putlog {[CMMF] End of List}
}

proc onln {name} {
    set ::tmp_onln $name
    ::furc::fd::1::puts S [format {onln %s} $name]
}

proc onln:event {fd name state} {
    if {![info exists ::tmp_onln]} { return 0 }
    if {[string tolower $name] != [string tolower $::tmp_onln]} { return 0 }

    if {!$state} {
        utimer 5 [format {onln %s} $name]
    } else {
    	${fd}::puts C [format {([N] %s is online!} $name]
	putlog [format {[FNOT] %s is online!} $name]
	unset ::tmp_onln
    }
    return 0
}

::furc::hook::add 3 S ONLN ::onln:event

proc bunnification {} {
    # Setting coords and frames-100 for bunnies #
    set bunny(x) {68 70 72 72 70 72 72 66 68 70 72 66 66 68 70 70 62 62 64 64 66 66 68 70 60 62 62 64 64 66 66 68 68 64 66 72 74 72 72 70 70 64 64 62 62}
    set bunny(y) {11 11 13 14 14 15 16 12 13 16 17 13 14 15 17 18 11 12 13 14 15 16 18 19 12 13 14 15 16 17 18 19 20 11 11 11 17 18 19 20 21 20 19 18 17}
    set bunny(f) {05 01 01 03 01 09 11 05 05 09 11 05 05 13 09 11 07 07 07 07 15 13 09 11 07 07 07 15 15 15 15 15 15 03 07 03 11 11 11 11 11 15 15 15 15}

    # Sending bunnies (this is gonna be scary!) #
    for {set i 0} {$i < [llength $bunny(x)]} {incr i} {
#	::furc::fd::broadcast C [format {<%s%s%s%s} {00?3?/////} [::furc::convert::itos [expr [lindex $bunny(x) $i] / 2]] [::furc::convert::itos [lindex $bunny(y) $i]] [::furc::convert::itos [format {1%s} [lindex $bunny(f) $i]]]]
	::furc::fd::broadcast C [format {]vb%s%s} [::furc::convert::itos [expr [lindex $bunny(x) $i] / 2]] [::furc::convert::itos [lindex $bunny(y) $i]]]
    }
    
    # Shirad Forgery #
#    ::furc::fd::1::puts C {]-#A"2J-=))))8!"!}
#    ::furc::fd::1::puts C {(Shirad: >..>;}
}

proc viewport {num} {
    ::furc::fd::1::puts C [format {]&%d pp%d} $num $num]
}

proc viewnext {} {
	incr ::portnum
	viewport $::portnum
}

proc flames {fd} {
    for {set i 0} {$i < 100} {incr i} {
	for {set j 0} {$j < 100} {incr j} {
	    ::furc::fd::${fd}::puts C [format {]vb%s%s} [::furc::convert::itos $i] [::furc::convert::itos $j]]
	}
    }
}

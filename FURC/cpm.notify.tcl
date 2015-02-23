##[1.0]##################[icedragon@quickfox.org]##
# cpm.notify.tcl #   Online Notification Module   #
###################################################
# This module was designed to track the online    #
# presense of multiple people.                    #
###################################################

namespace eval ::noti {
	# Variables #
	variable fuse    {1}   ;# Safety lock
	variable version {1.0} ;# Module version

	# Functions #
	proc add  {} {} ;# Add a name to the notify list
	proc del  {} {} ;# Remove a name from the notify list
	proc find {} {} ;# Check if a name is in the notify list

}

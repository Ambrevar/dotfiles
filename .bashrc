################################################################################
## Shell Config -- Master File
## Date 2011-11-19
################################################################################

##==============================================================================
## If not running interactively, don't do anything
##==============================================================================
[ -z "$PS1" ] && return


##==============================================================================
## Base functions
##==============================================================================

isShell()
{
    if [ "$1" = "$(ps | grep $$ | awk '{print $4}')" ]; then
        return 0
    else
        return 1
    fi
}

safeSource()
{
	if [ -f "$1" ]; then
		source "$1"
        return 0
	else
		echo "Warning: $1 not found when sourcing!"
        return 1
	fi
}


##==============================================================================
## Sourcing
##==============================================================================

SHELLDIR="$HOME/.shell.d"

safeSource "${SHELLDIR}/main_rc" # Should be sourced first.
safeSource "${SHELLDIR}/options_bash"
safeSource "${SHELLDIR}/funs_rc"
safeSource "${SHELLDIR}/funs_bash"
safeSource "${SHELLDIR}/alias_rc"
safeSource "${SHELLDIR}/personal_rc"

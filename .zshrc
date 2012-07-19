################################################################################
## Shell Config -- Master File
## Date 2012-07-02
################################################################################

##==============================================================================
## If not running interactively, don't do anything
##==============================================================================
## Useless for Zsh ?
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

##==============================================================================
## Sourcing
##==============================================================================

SHELLDIR="$HOME/.shell.d"

source "${SHELLDIR}/main_rc" # Should be sourced first.
source "${SHELLDIR}/options_zsh" # Should be sourced first.
source "${SHELLDIR}/colors_rc" # Should be sourced first.

source "${SHELLDIR}/funs_rc"
source "${SHELLDIR}/alias_rc"
source "${SHELLDIR}/keys_zsh"
source "${SHELLDIR}/personal_rc"


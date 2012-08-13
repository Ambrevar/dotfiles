################################################################################
## Shell Config -- Master File
## Date 2012-08-11
################################################################################

##==============================================================================
## If not running interactively, don't do anything
##==============================================================================
## Useless for Zsh ?
[ -z "$PS1" ] && return

##==============================================================================
## Sourcing
##==============================================================================

SHELL_CURRENT="$(ps | awk -v shellpid=$$ '$0 ~ shellpid {print $4}')"
SHELL_DIR="$HOME/.shell.d"

## Should be sourced first.
source "${SHELL_DIR}/main_rc"
source "${SHELL_DIR}/options_zsh"

## Source order should not matter.
source "${SHELL_DIR}/alias_rc"
source "${SHELL_DIR}/colors_zsh"    
source "${SHELL_DIR}/funs_rc"
source "${SHELL_DIR}/keys_zsh"
source "${SHELL_DIR}/personal_rc"


################################################################################
## Shell Config -- Master File
## Date 2012-08-11
################################################################################

##==============================================================================
## If not running interactively, don't do anything
##==============================================================================
[ -z "$PS1" ] && return

##==============================================================================
## Sourcing
##==============================================================================

SHELL_CURRENT="$(ps -o command="" $$)"
SHELL_DIR="$HOME/.shell.d"

## Should be sourced first.
source "${SHELL_DIR}/main_rc"
source "${SHELL_DIR}/colors_bash"    
source "${SHELL_DIR}/options_bash"

## Source order should not matter.
source "${SHELL_DIR}/alias_rc"
source "${SHELL_DIR}/funs_rc"
source "${SHELL_DIR}/funs_transcode"
source "${SHELL_DIR}/funs_bash"
source "${SHELL_DIR}/personal_rc"

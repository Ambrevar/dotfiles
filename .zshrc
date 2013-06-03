################################################################################
## Shell Config -- Master File
## Date 2012-08-11
################################################################################

##==============================================================================
## Sourcing
##==============================================================================

SHELL_CURRENT="$(ps -o command="" $$)"
SHELL_DIR="$HOME/.shell.d"

## Should be sourced first.
. "$HOME/.profile"
. "${SHELL_DIR}/main_rc"
. "${SHELL_DIR}/options_zsh"

## Source order should not matter.
. "${SHELL_DIR}/alias_rc"
. "${SHELL_DIR}/colors_zsh"
. "${SHELL_DIR}/completion_rc"
. "${SHELL_DIR}/funs_rc"
. "${SHELL_DIR}/keys_zsh"

## Should be sourced last
[ -f "${SHELL_DIR}/hook" ] && . "${SHELL_DIR}/hook"

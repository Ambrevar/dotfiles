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
source "${SHELL_DIR}/main_rc"
source "${SHELL_DIR}/options_zsh"

## Source order should not matter.
source "${SHELL_DIR}/alias_rc"
source "${SHELL_DIR}/colors_zsh"    
source "${SHELL_DIR}/funs_rc"
source "${SHELL_DIR}/keys_zsh"
source "${SHELL_DIR}/personal_rc"


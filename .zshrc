################################################################################
## Shell Config -- Master File
## Date 2012-08-11
################################################################################

##==============================================================================
## Sourcing
##==============================================================================

SHELL_CURRENT="$(ps -o command="" $$)"
SHELL_DIR="$HOME/.shell.d"

## Need to be expanded for external scripts to fetch zsh hook file location.
HOOK_FILE="$HOME/.shell.d/hook"

## .profile is sourced automatically when X is started, but we need to source it
## manually to TTY.
[ -z "$DISPLAY" ] && [ -f "$HOME/.profile" ] && . "$HOME/.profile"

## Should be sourced first.
[ -f "${SHELL_DIR}/main_rc" ] && . "${SHELL_DIR}/main_rc"
[ -f "${SHELL_DIR}/options_zsh" ] && . "${SHELL_DIR}/options_zsh"

## Source order should not matter.
[ -f "${SHELL_DIR}/alias_rc" ] && . "${SHELL_DIR}/alias_rc"
[ -f "${SHELL_DIR}/colors_zsh" ] && . "${SHELL_DIR}/colors_zsh"
[ -f "${SHELL_DIR}/completion_rc" ] && . "${SHELL_DIR}/completion_rc"
[ -f "${SHELL_DIR}/funs_rc" ] && . "${SHELL_DIR}/funs_rc"
[ -f "${SHELL_DIR}/keys_zsh" ] && . "${SHELL_DIR}/keys_zsh"

## Should be sourced last
[ -f "$HOOK_FILE" ] && . "$HOOK_FILE"

## Browser autostart. See .scripts/browser-autostart
if [ -n "$BROWSER_AUTOSTART" ]; then
    browse
fi

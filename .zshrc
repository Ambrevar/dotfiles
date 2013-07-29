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

loadrc()
{
    for i; do
        [ -f "${SHELL_DIR}/$i" ] && . "${SHELL_DIR}/$i"
    done
}

## main and options should be sourced first.
loadrc main_rc options_zsh

## Source order should not matter.
loadrc keys_zsh
loadrc alias_rc colors_rc completion_rc funs_rc

## Should be sourced last
loadrc hook

## Browser autostart. See .scripts/browser-autostart
[ -n "$BROWSER_AUTOSTART" ] && browse

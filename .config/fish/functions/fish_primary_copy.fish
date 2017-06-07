function fish_primary_copy
    if type -q pbcopy
        commandline | pbcopy
    else if type -q xsel
        commandline | xsel
    end
end

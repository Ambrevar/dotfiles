------------------------------------------------------------------------
-- Simple URI-based content filter                                    --
-- (C) 2010 Chris van Dijk (quigybo) <quigybo@hotmail.com>            --
-- (C) 2010 Mason Larobina (mason-l) <mason.larobina@gmail.com>       --
-- © 2012 Plaque FCC <Reslayer@ya.ru>                                 --
-- © 2010 adblock chromepage from bookmarks.lua by Henning Hasemann & --
-- Mason Larobina taken by Plaque FCC.                                --
--                                                                    --
-- Download an Adblock Plus compatible filter lists to luakit data    --
-- dir into "/adblock/" directory for multiple lists support or into  --
-- data dir root to use single file. EasyList is the most popular     --
-- Adblock Plus filter list: http://easylist.adblockplus.org/         --
-- Filterlists need to be updated regularly (~weekly), use cron!      --
------------------------------------------------------------------------

local info = info
local pairs = pairs
local ipairs = ipairs
local assert = assert
local unpack = unpack
local type = type
local io = io
local os = os
local string = string
local table = table
local tostring = tostring
local tonumber = tonumber
local webview = webview
local lousy = require("lousy")
local util = lousy.util
local chrome = require("chrome")
local capi = { luakit = luakit }
local add_binds, add_cmds = add_binds, add_cmds
local lfs = require("lfs")
local window = window

module("adblock")

--- Module global variables
local enabled = true
-- Adblock Plus compatible filter lists
local adblock_dir = capi.luakit.data_dir .. "/adblock/"

local filterfiles = {}
local simple_mode = true
local subscriptions_file = adblock_dir .. "/subscriptions"
local subscriptions = {}


-- Templates
header_template         = [==[<div class="header"><h2>AdBlock module: {state}</h2><br>AdBlock is in <b>{mode}</b> mode.{rules}</div><hr>]==]
rules_template          = [==[ {black} rules blacklisting, {white} rules whitelisting.]==]
block_template          = [==[<div class="tag"><h1>{opt}</h1><ul>{links}</ul></div>]==]
list_template_enabled   = [==[<li>{title}: <i>(b{black}/w{white}), </i> <a href="{uri}">{name}</a> <span class="id">{id}</span></li>]==]
list_template_disabled  = [==[<li>{title}: <a href="{uri}">{name}</a> <span class="id">{id}</span></li>]==]

html_template = [==[
<html>
<head>
    <title>{title}</title>
    <style type="text/css">
    {style}
    </style>
</head>
<body>
{header}
{opts}
</body>
</html>
]==]

-- Template subs
html_page_title = "AdBlock filters"

html_style = [===[
    body {
        font-family: monospace;
        margin: 25px;
        line-height: 1.5em;
        font-size: 12pt;
    }
    div.tag {
        width: 100%;
        padding: 0px;
        margin: 0 0 25px 0;
        clear: both;
    }
    span.id {
        font-size: small;
        color: #333333;
        float: right;
    }
    .tag ul {
        padding: 0;
        margin: 0;
        list-style-type: none;
    }
    .tag h1 {
        font-size: 12pt;
        font-weight: bold;
        font-style: normal;
        font-variant: small-caps;
        padding: 0 0 5px 0;
        margin: 0;
        color: #CC3333;
        border-bottom: 1px solid #aaa;
    }
    .tag a:link {
        color: #0077bb;
        text-decoration: none;
    }
    .tag a:hover {
        color: #0077bb;
        text-decoration: underline;
    }
]===]


-- String patterns to filter URI's with
local rules = {}

-- Functions to filter URI's by
-- Return true or false to allow or block respectively, nil to continue matching
local filterfuncs = {}

-- Enable or disable filtering
enable = function ()
    enabled = true
    refresh_views()
end
disable = function ()
    enabled = false
    refresh_views()
end

-- Report AdBlock state: «Enabled» or «Disabled»
state = function()
    if enabled then
        return "Enabled"
    else
        return "Disabled"
    end
end

mode = function()
    if simple_mode then
        return "simple"
    else
        return "normal"
    end
end

-- Detect files to read rules from
function detect_files()
    local curdir = lfs.currentdir()
    -- Try to find subscriptions directory:
    if not lfs.chdir(adblock_dir) then
        lfs.mkdir(adblock_dir)
    else
        simple_mode = false
        -- Look for filters lists:
        lfs.chdir(curdir)
        for filename in lfs.dir(adblock_dir) do
            if string.find(filename, ".txt$") then
                info("adblock: Found adblock list: " .. filename)
                table.insert(filterfiles, filename)
            end
        end
    end
    
    if table.maxn(filterfiles) < 1 then
        simple_mode = true
        filterfiles = { "/easylist.txt" }
    end
    
    if not simple_mode then
        info( "adblock: Found " .. table.maxn(filterfiles) .. " rules lists.\n" )
    end
    
    return
end

-- Convert Adblock Plus filter description to lua string pattern
-- See http://adblockplus.org/en/filters for more information
abp_to_pattern = function (s)
    -- Strip filter options
    local opts
    local pos = string.find(s, "%$")
    if pos then
        s, opts = string.sub(s, 0, pos-1), string.sub(s, pos+1)
    end

    -- Protect magic characters (^$()%.[]*+-?) not used by ABP (^$()[]*)
    s = string.gsub(s, "([%%%.%+%-%?])", "%%%1")

    -- Wildcards are globbing
    s = string.gsub(s, "%*", "%.%*")

    -- Caret is separator (anything but a letter, a digit, or one of the following:Â - . %)
    s = string.gsub(s, "%^", "[^%%w%%-%%.%%%%]")

    -- Double pipe is domain anchor (beginning only)
    -- Unfortunately "||example.com" will also match "wexample.com" (lua doesn't do grouping)
    s = string.gsub(s, "^||", "^https?://w?w?w?%%d?%.?")

    -- Pipe is anchor
    s = string.gsub(s, "^|", "%^")
    s = string.gsub(s, "|$", "%$")

    -- Convert to lowercase ($match-case option is not honoured)
    s = string.lower(s)
    

    return s
end

-- Parses an Adblock Plus compatible filter list
parse_abpfilterlist = function (filename)
    if os.exists(filename) then
        info("adblock: loading filterlist %s", filename)
    else
        info("adblock: error loading filter list (%s: No such file or directory)", filename)
    end
    local pat
    local white, black = {}, {}
    for line in io.lines(filename) do
        -- Ignore comments, header and blank lines
        if line:match("^[![]") or line:match("^$") then
            -- dammitwhydoesntluahaveacontinuestatement

        -- Ignore element hiding
        elseif line:match("#") then

        -- Check for exceptions (whitelist)
        elseif line:match("^@@") then
            pat = abp_to_pattern(string.sub(line, 3))
            if pat and pat ~= "^http://" then
                table.insert(white, pat)
            end

        -- Add everything else to blacklist
        else
            pat = abp_to_pattern(line)
            if pat and pat ~= "^http:" and pat ~= ".*" then
                table.insert(black, pat)
            end
        end
    end

    return white, black
end

-- Refresh open filters views (if any)
function refresh_views()
    for _, w in pairs(window.bywidget) do
        for _, v in ipairs(w.tabs.children) do
            if string.match(v.uri, "^luakit://adblock/?") then
                v:reload()
            end
        end
    end
end

-- Load filter list files
load = function (reload, single_list)
    if reload then subscriptions, filterfiles = {}, {} end
    detect_files()
    if not simple_mode and not single_list then
        read_subscriptions()
        local files_list = {}
        for _, filename in ipairs(filterfiles) do
            local list = subscriptions[filename]
            if list and util.table.hasitem(list.opts, "Enabled") then
                table.insert(files_list, filename)
            else
                add_list("", filename, "Disabled", true, false)
            end
        end
        filterfiles = files_list
        -- Yes we may have changed subscriptions and even fixed something with them.
        write_subscriptions()
    end

    -- [re-]loading:
    if reload then rules = {} end
    local filters_dir = adblock_dir
    if simple_mode then
        filters_dir = capi.luakit.data_dir
    end
    local filterfiles_loading = {}
    if single_list and not reload then
        filterfiles_loading = { single_list }
    else
        filterfiles_loading = filterfiles
    end
    for _, filename in ipairs(filterfiles_loading) do
        local white, black = parse_abpfilterlist(filters_dir .. filename)
        local list = {}
        if not simple_mode then
            list = subscriptions[filename]
        else
            local list_found = rules[filename]
            if list_found then
                list = list_found
            end
        end
        if not util.table.hasitem(rules, list) then
            rules[filename] = list
        end
        list.title, list.white, list.black = filename, table.maxn(white) or 0, table.maxn(black) or 0
        list.whitelist, list.blacklist = white or {}, black or {}
    end
    
    refresh_views()
end

-- Tests URI against user-defined filter functions, then whitelist, then blacklist
match = function (uri, signame)
    -- Matching is not case sensitive
    uri = string.lower(uri)
    signame = signame or ""

    -- Test uri against filterfuncs
    for _, func in ipairs(filterfuncs) do
        local ret = func(uri)
        if ret ~= nil then
            info("adblock: filter function %s returned %s to uri %s", tostring(func), tostring(ret), uri)
            return ret
        end
    end
    
    -- Test against each list's whitelist rules first
    for _, list in pairs(rules) do
        -- Check for a match to whitelist
        for _, pattern in ipairs(list.whitelist or {}) do
            if string.match(uri, pattern) then
                info("adblock: allowing %q as pattern %q matched to uri %s", signame, pattern, uri)
                return true
            end
        end
    end
    
    -- Test against each list's blacklist rules
    for _, list in pairs(rules) do
        -- Check for a match to blacklist
        for _, pattern in ipairs(list.blacklist or {}) do
            if string.match(uri, pattern) then
                info("adblock: blocking %q as pattern %q matched to uri %s", signame, pattern, uri)
                return false
            end
        end
    end
end

-- Direct requests to match function
filter = function (v, uri, signame)
    if enabled then return match(uri, signame or "") end
end

function table.itemid(t, item)
    local pos = 0
    for id, v in pairs(t) do
        pos = pos + 1
        if v == item then
            return pos
        end
    end
end

-- Connect signals to all webview widgets on creation
webview.init_funcs.adblock_signals = function (view, w)
    view:add_signal("navigation-request",        function (v, uri) return filter(v, uri, "navigation-request")        end)
    view:add_signal("resource-request-starting", function (v, uri) return filter(v, uri, "resource-request-starting") end)
end

-- Remove options and add new ones to list
-- @param list_index Index of the list to modify
-- @param opt_ex Options to exclude
-- @param opt_inc Options to include
function list_opts_modify(list_index, opt_ex, opt_inc)
    assert( simple_mode == false, "adblock list management: not supported in simple mode" )
    assert(type(list_index) == "number", "list options modify: invalid list index")
    assert(list_index > 0, "list options modify: index has to be > 0")
    if not opt_ex then opt_ex = {} end
    if not opt_inc then opt_inc = {} end
    
    if type(opt_ex) == "string" then opt_ex = util.string.split(opt_ex) end
    if type(opt_inc) == "string" then opt_inc = util.string.split(opt_inc) end
    
    local list = util.table.values(subscriptions)[list_index]
    local opts = opt_inc
    for _, opt in ipairs(list.opts) do
        if not util.table.hasitem(opt_ex, opt) then
            table.insert(opts, opt)
        end
    end
    
    -- Manage list's rules
    local listIDfound = table.itemid(rules, list)
    if util.table.hasitem(opt_inc, "Enabled") then
        if not listIDfound then
            load(false, list.title)
        end
    elseif util.table.hasitem(opt_inc, "Disabled") then
        rules[list.title] = nil
    end
    
    list.opts = opts
    write_subscriptions()
    refresh_views()
end

--- Add a list to the in-memory lists table
function add_list(uri, title, opts, replace, save_lists)
    assert( (title ~= nil) and (title ~= ""), "adblock list add: no title given")
    if not opts then opts = {} end

    -- Create tags table from string
    if type(opts) == "string" then opts = util.string.split(opts) end
    if table.maxn(opts) == 0 then table.insert(opts, "Disabled") end
    if not replace and ( subscriptions[title] or subscriptions[uri] ) then
        local list = subscriptions[title] or subscriptions[uri]
        -- Merge tags
        for _, opts in ipairs(opts) do
            if not util.table.hasitem(list, opts) then table.insert(list, opts) end
        end
    else
        -- Insert new adblock list
        local list = { uri = uri, title = title, opts = opts }
        if not (uri == "" or uri == nil) then
            subscriptions[uri] = list
        end
        if not (title == "" or title == nil) then
            subscriptions[title] = list
        end
    end

    -- Save by default
    if save_lists ~= false then write_subscriptions() end
end

--- Save the in-memory subscriptions to flatfile.
-- @param file The destination file or the default location if nil.
function write_subscriptions(file)
    if not file then file = subscriptions_file end

    local lines = {}
    local added = {}
    for _, list in pairs(subscriptions) do
        if not util.table.hasitem(added, list) then
            local subs = { uri = list.uri, title = list.title, opts = table.concat(list.opts or {}, " "), }
            local line = string.gsub("{title}\t{uri}\t{opts}", "{(%w+)}", subs)
            table.insert(added, list)
            table.insert(lines, line)
        end
    end

    -- Write table to disk
    local fh = io.open(file, "w")
    fh:write(table.concat(lines, "\n"))
    io.close(fh)
end

--- Load subscriptions from a flatfile to memory.
-- @param file The subscriptions file or the default subscriptions location if nil.
-- @param clear_first Should the subscriptions in memory be dumped before loading.
function read_subscriptions(file, clear_first)
    if clear_first then clear() end

    -- Find a subscriptions file
    if not file then file = subscriptions_file end
    if not os.exists(file) then return end

    -- Read lines into subscriptions data table
    for line in io.lines(file or subscriptions_file) do
        local title, uri, opts = unpack(util.string.split(line, "\t"))
        if title ~= "" then add_list(uri, title, opts, false, false) end
    end
end

--- Shows the chrome page in the given view.
chrome.add("adblock/", function (view, uri)
    -- Get a list of all the unique tags in all the lists and build a
    -- relation between a given tag and a list of subscriptions with that tag.
    local opts = {}
    local id = 0
    for _, list in pairs(subscriptions) do
        id = id + 1
        list['id'] = id
        for _, opt in ipairs(list.opts) do
            if not opts[opt] then opts[opt] = {} end
            opts[opt][list.title] = list
        end
    end

    -- For each opt build a block
    local lines = {}
    for _, opt in ipairs(util.table.keys(opts)) do
        local links = {}
        for _, title in ipairs(util.table.keys(opts[opt])) do
            local list = opts[opt][title]
            local link_subs = {
                uri     = list.uri,
                id      = list.id,
                name    = util.escape(list.uri),
                title   = list.title,
                white   = list.white,
                black   = list.black,
            }
            local list_template = list_template_disabled
            -- Show rules count only when enabled this list and have read its rules
            if util.table.hasitem(list.opts, "Enabled") and list.white and list.black then
                -- For totals count items only once (protection from multi-tagging by several opts confusion)
                list_template = list_template_enabled
            end
            local link = string.gsub(list_template, "{(%w+)}", link_subs)
            table.insert(links, link)
        end

        local block_subs = {
            opt   = opt,
            links = table.concat(links, "\n")
        }
        local block = string.gsub(block_template, "{(%w+)}", block_subs)
        table.insert(lines, block)
    end
    
    local rulescount = { black = 0, white = 0 }
    for _, list in pairs(rules) do
        if list.black and list.white then
            rulescount.black, rulescount.white = rulescount.black + list.black, rulescount.white + list.white
        end
    end
    -- Display rules count only if have them been count
    local html_rules = ""
    if rulescount.black + rulescount.white > 0 then
        html_rules = string.gsub(rules_template, "{(%w+)}", rulescount)
    end
    -- Fill the header
    local header_subs = {
        state = state(),
        mode  = mode(),
        rules = html_rules,
    }
    local html_page_header = string.gsub(header_template, "{(%w+)}", header_subs)

    local html_subs = {
        opts   = table.concat(lines, "\n\n"),
        title  = html_page_title,
        header = html_page_header,
        style  = html_style,
    }

    local html = string.gsub(html_template, "{(%w+)}", html_subs)
    view:load_string(html, tostring(uri))
end)

-- URI of the chrome page
chrome_page    = "luakit://adblock/"

-- Add normal binds.
local key, buf = lousy.bind.key, lousy.bind.buf
add_binds("normal", {
    buf("^ga$", function (w)
        w:navigate(chrome_page)
    end),

    buf("^gA$", function (w, b, m)
        for i=1, m.count do
            w:new_tab(chrome_page)
        end
    end, {count=1}),
})


-- Add commands.
local cmd = lousy.bind.cmd
add_cmds({
    cmd({"adblock-reload", "abr"}, function (w)
        info("adblock: Reloading filters.")
        load(true)
        info("adblock: Reloading filters complete.")
    end),
    
    cmd("adblock", function (w)
        w:navigate("luakit://adblock/")
    end),
    
    cmd({"adblock-list-enable", "able"}, function (w, a)
        list_opts_modify(tonumber(a), "Disabled", "Enabled")
    end),
    
    cmd({"adblock-list-disable", "abld"}, function (w, a)
        list_opts_modify(tonumber(a), "Enabled", "Disabled")
    end),
    cmd({"adblock-enable", "abe"}, function (w)
	enable()
    end),
    
    cmd({"adblock-disable", "abd"}, function (w)
	disable()
    end),
})

-- Initialise module
load()

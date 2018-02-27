--[[ Demlo configuration file
All the commandline options can get a default value from here.
Commandline values take precedence.
--]]

-- Colors may not work on all terminals.
Color = true

-- Number of cores to use (0 for all).
Cores = 0

--[[ When the destination exit, the "exist" action is taken.
An action is a Lua script which sets the variable 'output.write' to the following possible values:
- "overwrite": overwrite.
- "skip": don't do anything.
- nil (or anything else): append a random suffix to the new file.

The following variable specifies the path or name of the default action script.
--]]
Exist = ''

-- Extensions to look for when a folder is browsed.
-- The following variable is a map which keys are the extensions and the values are 'true'.
Extensions = {}
ext = {'aac', 'ape', 'flac', 'm4a', 'mp3', 'mp4', 'mpc', 'ogg', 'wav', 'wv'}
for _, v in ipairs(ext) do
	Extensions[v]=true
end

-- Whther to fetch cover from an online database.
-- Since Internet queries slow down the process, it's recommended to only turn
-- it on from the commandline when needed.
Getcover = false

-- Fetch tags from an online database.
-- Since Internet queries slow down the process, it's recommended to only turn
-- it on from the commandline when needed.
Gettags = false

-- Lua code to run before and after the other scripts, respectively.
Prescript = ''
Postscript = ''

-- If false, show preview and exit before processing.
Process = false

-- Scripts to run by default.
-- Scripts can later be added or removed via the commandline.
-- Demlo runs them in lexicographic order.
-- Thus the names matter, e.g. 'path' can be influenced by the modifications
-- made by 'tag', so we name the scripts with a prefix number so that 'tag' is
-- run before 'path'.
Scripts = {'10-tag-normalize', '15-tag-disc_from_path', '20-tag-replace', '30-tag-case', '40-tag-punctuation', '50-encoding', '60-path', '70-cover',
	'00-const', '51-encoding-flac2ogg', '59-humour'}

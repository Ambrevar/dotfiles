-- X-Ray Dog
-- WARNING: Untested.

-- Remove all default tags.
tags = {}

tags.artist = 'X-Ray Dog'
-- Albums actually have years, but is it relevent since they have a code?
tags.date = ''
-- Genre could be Soundtrack, or Trailer Music. We choose '' for now.
tags.genre = ''

-- Extract album code, assuming parent folder is 'XRCD## - $album'.
XRCD = output.filename:match('/[^/]*(XRCD\d*)[^/]*/[^/]*$')

-- Append the album code to the constants array, other it will be capitalized.
constants [XRCD:upper()] = XRCD

tags.album = not empty (o.album) and o.album:gsub ('XRCD - (\d+)', 'XRCD$1 - ') or 'Unknown album'

tags.title = o.title
if empty(tags.title) then
	tags.title = output.filename:match('[^/]+$'):gsub('\..+$', ''):gsub('X-Ray Dog[\s*?]*', '')
end

tags.track = o.track
if empty(tags.track) then
	tags.track = o.title:match('\d+')
end

output.tags = tags

-- X-Ray Dog
-- WARNING: untested.

-- Remove all default tags.
output.tags = {}
o = output.tags

o.artist = 'X-Ray Dog'
-- Albums actually have years, but is it relevent since they have a code?
o.date = ''
-- Genre could be Soundtrack, or Trailer Music. We choose '' for now.
o.genre = ''

-- Extract album code, assuming parent folder is 'XRCD## - $album'.
XRCD = i.filename:match ('/[^/]*(XRCD%d*)[^/]*/[^/]*$')

-- Append the album code to the constants array, other it will be capitalized.
constants [XRCD:upper ()] = XRCD

o.album = i.album
o.album:gsub ('XRCD - (%d+)', 'XRCD%1 - ')

o.title = i.title
if empty (o.title) then
   o.title = i.filename:match ('[^/]+$')
   o.title = o.title:gsub ('%..+$', '')
   o.title = o.title:gsub ('X-Ray Dog[%s-]*', '')
end

o.track = i.track
if empty (o.track) then
   o.track = o.title:match ('%d+')
end

cleantags (o)

-- Output filename
output.filename = table.concat {library, '/', o.artist, '/',
             not empty (o.album) and (empty (o.date) and o.album .. '/' or o.date .. ' - ' .. o.album .. '/') or '',
             empty (track_padded) and '' or track_padded .. ' - ',
             o.title}

-- Options
if o.album then
   output.cover_basename = o.album .. ' - Cover'
else
   output.cover_basename = 'Cover'
end
output.cover_clear_embedded = true
output.cover_discard_high = false
output.cover_discard_low = true
output.cover_limit_low = 100
output.tags_clear = true

-- Soundtrack tags.
if stringrel(stringnorm (o.genre), 'soundtrack') > 0.7 then
	o.album_artist = o.album
end

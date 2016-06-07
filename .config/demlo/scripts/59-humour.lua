-- Output 'humour' genre to a separate library.
-- Run before 'path'.

local ossep = osseperator or '/'

local genre = o.genre and o.genre:lower():gsub([[\s]],'_')
if genre and genre == 'humour' then
	library = os.getenv('HOME')  .. ossep .. 'humour'
else
	library = os.getenv('HOME')  .. ossep .. 'music'
end

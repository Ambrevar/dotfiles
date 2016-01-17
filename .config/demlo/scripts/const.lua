-- Constants

const = const or {}

const_custom = {
	'AC',
	'BYOB',
	'DC',
	'DMX',
	'FBI',
	'MGMT',
	'RZA',
	'SWAT',
	'USA',
	'USSR',
	'AIDS',
	'DNA',
	'DVD',
	'FM',
	'JFK',
	'JBX',
	"d'Acide",

	--French
	'avec',
	'chez',
	'dans',
	'de',
	'des',
	'du',
	'le',
	'la',
	'les',
	'que',
	'qui',
	'quoi',
	'sur',
	'un',
	'une',

	-- German
	'der',
	'die',
	'das',
	'den',
	'dem',
	'des',
	'ein',
	'eine',
	'einen',
	'eines',
	'einer',
	'einem',
	'im',
	'wo',
	'an',
	'am',
	'in',
	'bei',
	'aus',
	'mit',
	'nach',
	'seit',
	'von',
	'zu',
	'vom',
	'zur',
	'zum',
	'durch',
	'für',
	'gegen',
	'ohne',
	'um',

	-- Spanish
	'y',
}

for _, v in ipairs(const_custom) do
	const[#const+1] = v
end

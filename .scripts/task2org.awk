#!/bin/gawk -f

## This script convert Task Warrior pending.data file to an org-mode compatible
## format. WARNING: unpolished work. Use at your own risk!

BEGIN {
	FS="\" "

	tagsep="    "
	project_empty="<none>"

	prio_array["H"]="[#A]"
	prio_array["M"]="[#B]"
	prio_array["L"]="[#C]"

	recur_array["yearly"]="+1y"
	recur_array["monthly"]="+1m"
	recur_array["weekly"]="+1w"
	recur_array["daily"]="+1d"
}

{
	gsub(/^\[|\]$/,"")
	gsub(/:"/,":")

	description=""
	project=project_empty
	annotation=""
	start=""
	priority=""
	recur=""
	due=""
	tags=""

	for (i = 1; i <= NF; i++) {
		split($i, a, ":")

		if (a[1] ~ "^description$")
			description=a[2] " "
		else if (a[1] ~ "^project$")
			project=a[2]
		else if (a[1] ~ "^annotation")
			annotation=annotation "\n   " a[2]
		else if (a[1] ~ "^priority$")
			priority=prio_array[a[2]] " "
		else if (a[1] ~ "^tags$")
			tags=tagsep ":" a[2] ": "
		else if (a[1] ~ "^start$")
			start="TODO "

		if (a[1] ~ "^recur$")
			recur=" " recur_array[a[2]]

		if (a[1] ~ "^due$")
			due="<" strftime("%F",a[2])

		delete a
	}

	## Close 'due' after loop in case 'recur' was encountered after 'due'.
	if (due != "")
		due = due recur "> "

	result[project]= result[project] "\n** " start priority description due tags annotation

}

function capitalize(s) {
	return toupper(substr(s,1,1)) tolower(substr(s,2))
}

END {
	for ( var in result)
		printf ("\n* %s%s\n", capitalize(var), result[var])
}

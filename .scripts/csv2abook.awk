## Google Contacts CSV to Abook.

## WARNING: this script will NOT work for entries that have a comma between double quotes.

BEGIN {
    FS=","
    ind=1
}

## Some fields are surrounded by unwanted double quotes.
function remove_quotes(s)
{
    gsub(/^"+|"+$/, "", s)
    return s
}

function capitalize(s)
{
    result = ""
    while(match(s,/[ -]/))
    {
        word = substr(s,1,RSTART-1)
        result = result toupper(substr(word,1,1)) tolower(substr(word,2))
        if(index(s,"-"))
             result = result "-"
        else
            result = result " "
        s = substr(s,RSTART+1)
    }
    result = result toupper(substr(s,1,1)) tolower(substr(s,2))
    return result
}

## Let's skip the first line which is supposed to contain the CSV header.
FNR==1 {
    next
}

{
    print "[" ind "]"
    print "name=" capitalize(remove_quotes($2)) " " capitalize(remove_quotes($4))
    printf "email=" tolower(remove_quotes($29))
    if ($31 != "")
        printf "," remove_quotes($31)
    if ($33 != "")
        printf "," remove_quotes($33)
    printf "\n"

    print "birthday=" remove_quotes($15)
    print "mobile=" remove_quotes($35)
    print "phone=" remove_quotes($37)
    print "address=" remove_quotes($39)
    print ""
    ind++
}


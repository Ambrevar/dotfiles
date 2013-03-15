## Google Reader to plain text.

## This AWK script will convert the starred.json file you get with Google
## Takeout from your Google Reader account to a plain text file.

BEGIN { FS=":" }

/^    "title" :/ {
    title=$2
    gsub(/ "|",/,"",title)
}

/^      "href" :/ {
    link=$2 ":" $3
    gsub(/ "|",/,"",link)
}

/\}, \{/ { print link " " title }

END { print link " " title }

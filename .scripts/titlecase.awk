#!/bin/gawk -f
## This script is inspired by
##   http://www.pement.org/awk/titlecase.awk.txt

##
## function: titlecase("CHANGE TO TITLE CASE") --> "Change to Title Case"
##
## Other Features:
##
##   titlecase() will compress whitespace if a second parameter is passed.  It
##   is sufficient to use a positive number: titlecase(string,1)
##
##   This function tries to implement the "Title Case" constructs specified in
##   the APA Style Manual and the Chicago Manual of Style. Instead of merely
##   capitalizing the first letter of each word and setting everything else in
##   lowercase, this function implements the following conditions:
##
##  - Conjunctions, articles, and prepositions are set lowercase, UNLESS they
##    are the first word of the string or the first word after a colon, a
##    question mark, or an exclamation point.
##  - Compass points (NE, SW, etc.) are set in solid caps.
##  - Roman numerals (II, IV, VII, IX, etc.) are set in solid caps.
##  - Certain abbreviations are always capitalized (AIDS, ASCII, NT, USA, etc.)
##  - Names beginning with D' or O' are set as D'Arcy, O'Reilly, etc.
##  - Hyphenated strings receive internal caps (Smith-Williams, Twenty-Two)
##  - Contractions such as I'll, You've, Don't, etc. are handled properly
##  - Degrees such as Ph.D., M.Div., etc. are properly capitalized
##
## Sample Usage with GNU awk (gawk):
##
##   gawk -f titlecase.awk infile

## TODO: maybe it would be a good idea to implement a preprocessor that would
## search and replace special strings like AC-DC.

## Tests:

## all lowercase words
## ALL UPPERCASE WORDS
## aLl cRaZY cASE WordS
## And with constants in an INTO cd Contre. Feat and Feat. the machine.
## Bad   ,punctuation. here  , should ! not be ?a problem.
## Roman numerals XIV LIV xiv liv. liv. xiv.
## Dashed--machine--ac-dc.
## About mcdonald and o'reilly, but i'll won't say.
## The "final quote" 'on the waterfront'.

BEGIN {
    ## English
    constants = constants "a an the and but for nor or so am is are against at between by from in into of on to upon "

    ## French
    constants = constants "un une de du le la les et mais pour ni ou à a où contre entre chez dans sur "

    ## German
    constants = constants "der die das den dem des ein eine einen eines einer von wo an am in für gegen bei aus mit nach seit zu durch ohne um "

    ## Music
    constants = constants "feat CD DJ "
    constants = constants "KlassX Machine d'Acide BYOB MGMT AC DC JBX RZA DMX "

    ## Others
    constants = constants "AIDS ASCII DHTML DNA DVD FBI GNU GPL IBM IRS ISBN ISSN PHP ROM SSN TV FM "

    ## Build array of constant words.
    split(constants, constarray, " ")
}

function titlecase(string)  {
    ## Initialize variables.
    a = "";            # a is/will be the string ALREADY converted
    b = string;        # b is the rest of the string, so that (string = a b)

    ## English punctuation. It is quite hard to guess the language, so French
    ## will follow English punctuation rules.
    b = gensub(/ +([,!:;?.]+) */, "\\1 ", "g", b)

    ## Compress spaces or tabs. Trim prefix and suffix space.
    gsub(/[ \t]+/, " ", b)
    gsub(/^ /, "", b)
    gsub(/ $/, "", b)

    ## Capitalize everything for ease of matching.
    b = toupper(b)

    do {
        ## Initialize for later use.
        hit = 0;

        ## 'pos' is the position of the NEXT punctuation mark (except
        ## apostrophe) after the current word. If this is the last word in b,
        ## pos will be 0.  match() automatically sets RLENGTH.  WARNING: we
        ## consider digits as part of a word.
        pos = match(b, /[^[:alnum:]']+/)

        if (pos > 0)    word = substr(b, 1, pos + RLENGTH - 1)
        else            word = b

        ## 1st char of current word.
        head = substr(b, 1, 1)
        ## Tail of current word.
        if (pos > 0)    tail = substr(b, 2, pos + RLENGTH - 2)
        else            tail = substr(b, 2)

        ## Shorten the rest of the string.
        b = substr(b, pos + RLENGTH  )

        ## RULE 1 -- Constant strings.

        ## WARNING: since we match a substring of 'word', we need to prepend and
        ## append the potentially discarded values, like dashes.
        for (var in constarray) {
            if (debug)
                print ":: Comparing " word " with " constarray[var]
            hit = match(word, "^" toupper(constarray[var]) "\\>")
            if ( hit > 0 ) {
                word = substr(word, 1, RSTART-1) constarray[var] substr(word, RSTART+RLENGTH)
                if (debug)
                    print ":: Match constant on [" constarray[var] "] in string [" word "]";
                break;
            }
        }

        ## RULE 2 -- Roman numerals

        ## Note: this match cannot distinguish between LIV (54 in Roman
        ## numerals) and a personal name like "Liv Ullman".  The Roman numerals
        ## C (100), D (500), and M (1000) are omitted to avoid false matches on
        ## words like civil, did, dim, lid, mid-, mild, Vic, etc. Most uses of
        ## Roman numerals in titles stays in the lower ranges, such as "Vol. II"
        ## or "Pt. XXIV".
        if ( hit == 0 && match(word, /^[IVXL]+\>/) ) {
            hit = 1
            ## But we can undo I'd, I'll, I'm, I've and Ill.
            if (match(word,/^I'|ILL\>/))
                hit = 0
            if (debug && hit == 1)
                print ":: Match on Roman numerals in [" word "]"
        }

        ## RULE 3 -- Names like D'Arcy or O'Reilly
        if ( hit == 0 && match(word, /^[DO]'[[:alpha:]]/) ) {
            word = substr(word,1,3) tolower(substr(word,4))
            hit = 1
            if (debug)
                print ":: Match on mixed case: " word
        }

        ## RULE 4 -- Names like MacNeil or McDonald
        if ( hit == 0 && match(word,/^MA?C[B-DF-HJ-NP-TV-Z]/) ) {
            if (debug)
                print  ":: Match on MacX: " substr(word,1,1) "-" \
                    tolower(substr(word,2,RLENGTH-2)) "-" substr(word,RLENGTH,1) "-" \
                    tolower(substr(word,RLENGTH+1))
            word = substr(word,1,1)       tolower(substr(word,2,RLENGTH-2)) \
                substr(word,RLENGTH,1) tolower(substr(word,RLENGTH+1))
            hit = 1
        }

        ## If one of the above rule is hit, we append the result to 'a',
        ## otherwise we capitalize it.
        if (hit > 0) a = a word
        else         a = a toupper(head) tolower(tail)

    } while (pos > 0);

    ## Everything should be converted now.

    ## Double exception 1: Set 1st word of string in capital case. Need to
    ## handle potential internal single/double quotes like "A Day in the Life"
    ## or 'On the Waterfront'. WARNING: here we consider digits as part of a
    ## word (as in 1st, 2nd, etc.).
    match(a, /[[:alnum:]]/)
    a = toupper(substr(a, 1, RSTART)) substr(a, RSTART+1)

    ## Double exception 2: Set 1st word after a colon, question mark,
    ## double-quotes or exclamation point in title case. This kludge handles
    ## multiple colons, question marks, etc. on the line. \a is the BEL or
    ## CTRL-G character.
    result = gensub(/([:?!"][^[:alnum:]]*)([a-zA-Z])/, "\\1\a\\2", "g", a)
    while (match(result, /\a/)) {
        beg = substr(result, 1, RSTART-1)
        cap = toupper(substr(result, RSTART+1, 1))
        end = substr(result, RSTART+2)
        result = beg cap end
    }

    return result
}

{print titlecase($0)}

## End of script

# filename: titlecase.awk

## Original file can be found at
##   http://www.pement.org/awk/titlecase.awk.txt

#
# function: titlecase("CHANGE TO TITLE CASE") --> "Change to Title Case"
#
# Other Features:
#   titlecase() will compress whitespace if a second parameter is passed.
#   It is sufficient to use a positive number: titlecase(string,1)
#
#   This function tries to implement the "Title Case" constructs specified
#   in the APA Style Manual and the Chicago Manual of Style. Instead of
#   merely capitalizing the first letter of each word and setting
#   everything else in lowercase, this function implements the following
#   conditions:
#
#  - Conjunctions, articles, and prepositions are set lowercase, UNLESS they
#    are the first word of the string or the first word after a colon, a
#    question mark, or an exclamation point.
#  - Compass points (NE, SW, etc.) are set in solid caps.
#  - Roman numerals (II, IV, VII, IX, etc.) are set in solid caps.
#  - Certain abbreviations are always capitalized (AIDS, ASCII, NT, USA, etc.)
#  - Names beginning with D' or O' are set as D'Arcy, O'Reilly, etc.
#  - Hyphenated strings receive internal caps (Smith-Williams, Twenty-Two)
#  - Contractions such as I'll, You've, Don't, etc. are handled properly
#  - Degrees such as Ph.D., M.Div., etc. are properly capitalized
#
# Sample Usage with GNU awk (gawk):
#
#   awk -f titlecase.awk infile

BEGIN {

    #-----ABBREVIATIONS TO BE SET IN MIXEDCASE-----
    mixed = "KlassX Machine "
    split(mixed, keep_mixed, " ")

    #-----ABBREVIATIONS TO BE SET IN LOWERCASE-----
    articles     = "a an the "
    conjunctions = "and but for nor or so "
    verbs = "am is are "
    abbrevs = "feat "

    # Prepositions
    # Omitted: over (=finished), under, through, before, after
    preps = "against at between by from in into of on to upon "

    # Build array of words to be set lowercased
    split(articles conjunctions preps verbs abbrevs, keep_lower, " ")

    #-----ABBREVIATIONS TO BE SET IN SOLID CAPS-----
    # Other abbreviations - add to this list as needed
    other =       "AIDS ASCII CD DHTML DNA DVD FBI GNU GPL IBM IRS ISBN ISSN "
    other = other "PHP ROM SSN TV FM BYOB MGMT DJ AC-DC JBX RZA DMX "

    # build array of words to keep uppercase
    split(other, keep_upper, " ")

}

function titlecase(string,x)  {

    # Initialize variables
    a = "";            # a is/will be the string ALREADY converted
    b = string;        # b is the rest of the string, so that (string = a b)
    compress = x;      # optional compression argument

    # Compress spaces or tabs if 2nd argument passed. Trim prefix and suffix space.
    if (compress) {
        gsub(/[ \t]+/, " ", b)
        gsub(/^ /, "", b)
        gsub(/ $/, "", b)
        if (debug) print "DIAG: Compress argument passed to function call"
    }

    b = toupper(b)     # Capitalize everything for ease of matching

    do {
        hit = 0;         # Initialize for later use

        # pos is the position of the NEXT punctuation mark (except apostrophe)
        # after the current word. If this is the last word in b, pos will be 0.
        # match() automatically sets RLENGTH
        ## WARNING: we consider digits as part of a word.
        pos = match(b, /[^A-Z0-9']+/)
        # pos = match(b, /[^A-Z']+/)

        if (pos > 0)    word = substr(b, 1, pos + RLENGTH - 1)
        else            word = b

        # 1st char of current word
        head = substr(b, 1, 1)
        # tail of current word
        if (pos > 0)    tail = substr(b, 2, pos + RLENGTH - 2)
        else            tail = substr(b, 2)

        # shorten the rest of the string
        b = substr(b, pos + RLENGTH  )

        #----Words to keep mixedcase----
        for (var in keep_mixed) {
            mix = match(word, "^" toupper(keep_mixed[var]) "\\>")
            if ( mix > 0 ) {
                hit = 1
                word = keep_mixed[var]
                if (debug)
                    print "DIAG: Match MC on [" keep_mixed[var] "] in string [" word "]";
                break;
            }
        }

        #----Words to keep uppercase----
        # Case 1: abbreviations from the keep_upper array.
        if ( proect == 0) {
            for (var in keep_upper) {
                hit = match(word, "^" keep_upper[var] "\\>")
                if ( hit > 0 ) {
                    if (debug)
                        print "DIAG: Match UC on [" keep_upper[var] "] in string [" word "]";
                    break;
                }
            }
        }

        # Case 2: Roman numerals
        # Note: this match cannot distinguish between LIV (54 in Roman numerals)
        # and a personal name like "Liv Ullman".  The Roman numerals C (100),
        # D (500), and M (1000) are omitted to avoid false matches on words like
        # civil, did, dim, lid, mid-, mild, Vic, etc. Most uses of Roman numerals
        # in titles stays in the lower ranges, such as "Vol. II" or "Pt. XXIV".
        if ( hit == 0 && match(word, /^[IVXL]+\>/) ) {
            hit = 1
            # But we can undo I'd, I'll, I'm, I've and Ill.
            if (match(word,/^I'|ILL\>/)) hit = 0
            if (debug && hit == 1)
                print "DIAG: Match on Roman numerals in [" word "]"
        }

        #----Words to be set in MiXed case----
        # Case 3: Names like D'Arcy or O'Reilly
        if ( hit == 0 && match(word, /^[DO]'[A-Z]/) ) {
            if (debug) print "DIAG: Match on mixed case: " word
            word = substr(word,1,3) tolower(substr(word,4))
            hit = 1
        }

        # Case 4: Names like MacNeil or McDonald
        if ( hit == 0 && match(word,/^MA?C[B-DF-HJ-NP-TV-Z]/) ) {
            if (debug)
                print  "DIAG: Match on MacX: " substr(word,1,1) "-" \
                    tolower(substr(word,2,RLENGTH-2)) "-" substr(word,RLENGTH,1) "-" \
                    tolower(substr(word,RLENGTH+1))
            word = substr(word,1,1)       tolower(substr(word,2,RLENGTH-2)) \
                substr(word,RLENGTH,1) tolower(substr(word,RLENGTH+1))
            hit = 1
        }

        #----Words to set in lowercase----
        # Case 5: articles, conjunctions, prepositions from the keep_lower array
        if (hit == 0) {
            for (var2 in keep_lower) {
                hit = sub("^" toupper(keep_lower[var2]) "\\>", keep_lower[var2], word);
                if ( hit > 0 ) {
                    if (debug)
                        print "DIAG: Match LC on [" keep_lower[var2] "] in string [" word "]";
                    break;
                }
            }
        }

        #----Default: Capitalize everything else normally----
        if (mix > 0)    a = a word
        else if (hit > 0)    a = a word
        else            a = a toupper(head) tolower(tail)


    } while (pos > 0);

    ## Everything should be converted now.

    ## Double exception 1: Set 1st word of string in capital case. Need to
    ## handle potential internal single/double quotes like "A Day in the Life"
    ## or 'On the Waterfront'. WARNING: here we consider digits as part of a
    ## work (as in 1st, 2nd, etc.)
    match(a, /[A-Za-z0-9]/)
    a = toupper(substr(a,1,RSTART)) substr(a,RSTART+1)

    ## Double exception 2: Set 1st word after a colon, question mark or
    ## exclamation point in title case. This kludge handles multiple colons,
    ## question marks, etc. on the line. \a is the BEL or CTRL-G character.
    ## WARNING: we also follow double quotes by a capital.
    done = gensub(/([:?!"][^a-zA-Z]*)([a-zA-Z])/,"\\1\a\\2", "g", a)

    while (match(done,/\a/)) {
        beg = substr(done,1,RSTART-1)
        cap = toupper(substr(done,RSTART+1,1))
        end = substr(done,RSTART+2)
        done = beg cap end
    }

    return done
}


{print titlecase($0,1)}

#---end of awk script---

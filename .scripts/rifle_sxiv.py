#!/usr/bin/env python
# Compatible with ranger 1.6.*
#
# If only one file is selected, this script searches image files in a directory,
# opens them all with sxiv and sets the first argument to the first image
# displayed by sxiv.
#
# If a selection is on, this script will start sxiv over the selection only.
#
# This is supposed to be used in rifle.conf as a workaround for the fact that
# sxiv takes no file name arguments for the first image, just the number.  Copy
# this file somewhere into your $PATH and add this at the top of rifle.conf:
#
#   mime ^image, has sxiv, X, flag f = path/to/this/script -- "$@"
#
# This wrapper supports parameter, so if you want to start fullscreen and to fit
# all image to window, use
#
#   mime ^image, has sxiv, X, flag f = path/to/this/script -fs -- "$@"

# TODO: support for mimetypes.
# result = [a for a in filelist if v=mimetypes.guess_type(a)[0] and type(v) is str and v.find('image') != -1 ]

import sys
import os
import subprocess
import mimetypes
import re

def usage():
    print("Usage: " + re.sub(r".*/", "", sys.argv[0])  + " PICTURES")


def sxiv_singlefile(inputfile):
    # Turn to an absolute path
    if inputfile[0] != '/':
        inputfile = os.path.abspath(inputfile)

    inputdir = re.sub(r"/[^/]+$", "/", inputfile)
    filelist = os.listdir(inputdir)
    filename = inputfile

    ## Note: os.path.join seems to be slow.
    result = [ inputdir + a for a in filelist if re.search('.(bmp|gif|jpe?g|png)$', a, re.IGNORECASE) != None ]
    list.sort(result)

    ## We get the index of the first argument to know where sxiv should start the display.
    try:
        count = result.index(inputfile) + 1
    except ValueError:
        count = 1

    result = ["-n" + str(count), "--"] + result
    if parameters:
        result = parameters + result
    result = ["sxiv"] + result

    subprocess.call(result)

def sxiv_multifile(arglist):
    result = [ os.path.abspath(a) for a in arglist ]
    list.sort(result)

    result = ["--"] + result
    if parameters:
        result = parameters + result
    result = ["sxiv"] + result

    print(result)
    subprocess.call(result)



## MAIN
if len(sys.argv) == 1:
    usage_exit()

arglist = sys.argv
arglist.pop(0)

## Put all sxiv parameters in a string.
parameters = []
while len(arglist) != 0 and arglist[0] != "--" and arglist[0][0] == "-":
    parameters = parameters + [arglist[0]]
    arglist.pop(0)

if len(arglist) == 0:
    usage()
    sys.exit(0)

if arglist[0] == "--":
    arglist.pop(0)

if len(arglist) == 0:
    usage()
elif len(arglist) == 1:
    sxiv_singlefile(arglist[0])
elif len(arglist) >= 2:
    sxiv_multifile(arglist)

sys.exit(0)


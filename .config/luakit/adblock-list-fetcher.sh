#!/bin/bash

mkdir -p "$XDG_DATA_HOME/luakit"
wget 'https://easylist-downloads.adblockplus.org/easylist.txt' -O "$XDG_DATA_HOME/luakit/easylist.txt"

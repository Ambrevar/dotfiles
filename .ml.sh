#!/bin/sh

pwds=`gpg --decrypt ~/.pwds.gpg`
eval "$pwds"
exec mutt "$@"

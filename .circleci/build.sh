#!/usr/bin/env bash

# set auto-exit when error
set -e

# run lein test selectors
#lein test :quick :slow
lein test

# add git sha1 into resource file
git_branch=`git branch | grep \* | cut -d ' ' -f2`
git_short_sha1=`git rev-parse --short HEAD`
git_branch_short_sha1="$git_branch-$git_short_sha1"
echo "$git_short_sha1" >> resources/sha1.txt
echo "$git_branch_short_sha1" > resources/branch-sha1.txt

# package artifact
lein uberjar


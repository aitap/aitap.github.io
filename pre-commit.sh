#!/bin/sh -ev
git stash -k -q
./tplmk.pl
git stash pop -q

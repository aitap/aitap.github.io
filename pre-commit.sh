#!/bin/sh -ev
git stash -k -q
ttree -f ttreerc
git stash pop -q

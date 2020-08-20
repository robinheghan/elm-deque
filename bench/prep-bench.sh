#!/bin/sh
set -e

../node_modules/.bin/elm make VsOld.elm --output old.html --optimize

#!/bin/sh
set -e

../node_modules/.bin/elm make Main.elm --output index.html --optimize

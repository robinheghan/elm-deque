#!/bin/sh
set -e

../node_modules/.bin/elm make VsDeque.elm --output deque.html --optimize
#../node_modules/.bin/elm make VsList.elm --output list.html --optimize

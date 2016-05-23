#!/bin/sh

elm make src/elm/Main.elm --output=public/bundle.js

cp src/index.html public/

#!/bin/bash

set -e
set -o pipefail

ELM_MAKE_OPT='--optimize'
ELM_LIVE=false

while [ -n "$1" ]; do
    case "$1" in
    --start)
        ELM_LIVE=true
        ;;
    --debug)
        ELM_MAKE_OPT="--debug"
        ;;
    *)
        echo "Bad option : $1"
        exit 1
        ;;
    esac
    shift
done

set -u

npm install

mkdir -p dist

cp -a static/* dist/
cp -a public/* dist/

cp -a node_modules/bulma/css/bulma.min.css dist/
cp -a node_modules/bulma-extensions/bulma-tooltip/dist/css/bulma-tooltip.min.css dist/
cp -a node_modules/bulma-extensions/bulma-checkradio/dist/css/bulma-checkradio.min.css dist/

if $ELM_LIVE; then
    elm-live src/Main.elm --dir dist --port 8001 --open -- --output=dist/elm.js "$ELM_MAKE_OPT"
else
    elm make src/Main.elm --output=dist/elm.js "$ELM_MAKE_OPT"
fi

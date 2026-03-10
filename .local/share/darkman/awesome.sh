#!/bin/bash

config="$HOME/.config/awesome/rc.lua"

case "$1" in
    light)
        sed -i 's/dark.lua/light.lua/' "$config"
        ;;
    dark)
        sed -i 's/light.lua/dark.lua/' "$config"
        ;;
    *)
        echo "Usage: $0 {light|dark}"
        exit 1
        ;;
esac

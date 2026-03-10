#!/bin/sh
# The names for the Arc theme variations are ambiguous:
# "Darker" is actually LESS DARK than "Dark".

case "$1" in
    dark)
        theme="Arc-Dark"
        scheme="prefer-dark"
        icon="hicolor"
        ;;

    light)
        theme="Arc"
        scheme="prefer-light"
        icon="hicolor"
        ;;
    *)
        echo "Usage: $0 {light|dark}"
        exit 1
        ;;
esac

# this seems to have no effect, actually Adwaita is run.
gsettings set org.gnome.desktop.interface color-scheme $scheme
gsettings set org.gnome.desktop.interface gtk-theme $theme
gsettings set org.gnome.desktop.interface icon-theme $icon

# libadwaita not respecting the light/dark choice
export ADW_DEBUG_COLOR_SCHEME=$scheme

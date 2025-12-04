#!/bin/sh

export CALIBRE_USE_DARK_PALETTE=0

# this seems to have no effect, actually Adwaita is run.
gsettings set org.gnome.desktop.interface color-scheme prefer-light
gsettings set org.gnome.desktop.interface gtk-theme Arc
gsettings set org.gnome.desktop.interface icon-theme hicolor

# libadwaita not respecting the light/dark choice
ADW_DEBUG_COLOR_SCHEME=prefer-light

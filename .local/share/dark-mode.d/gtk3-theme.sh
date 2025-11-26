#!/bin/sh

export CALIBRE_USE_DARK_PALETTE=1

# this seems to have no effect, actually Adwaita-Dark is run.
gsettings set org.gnome.desktop.interface color-scheme prefer-dark
gsettings set org.gnome.desktop.interface gtk-theme Arc-BLACKEST
gsettings set org.gnome.desktop.interface icon-theme hicolor

#!/bin/sh

# Note: The names for the Arc theme variations are terrible.
# "Darker" is actually LESS DARK than "Dark".
#gsettings set org.gnome.desktop.interface gtk-theme Arc-Darker

# this seems to have no effect, actually Adwaita-Dark is run.
gsettings set org.gnome.desktop.interface gtk-theme Arc
gsettings set org.gnome.desktop.interface icon-theme hicolor

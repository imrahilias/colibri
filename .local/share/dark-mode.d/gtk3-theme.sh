#!/bin/sh
# Set GTK theme using gsettings

# Note: The names for the Arc theme variations are terrible.
# "Darker" is actually LESS DARK than "Dark".
#gsettings set org.gnome.desktop.interface gtk-theme Adwaita-dark

# mine
gsettings set org.gnome.desktop.interface gtk-theme Arc-BLACKEST
gsettings set org.gnome.desktop.interface color-scheme prefer-dark

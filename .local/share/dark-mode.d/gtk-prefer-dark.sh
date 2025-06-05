#/bin/bash

sed -i -e '/gtk-application-prefer-dark-theme/s/false/true/' "${XDG_CONFIG_HOME:-$HOME/.config}/gtk-3.0/settings.ini"

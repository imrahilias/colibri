#!/bin/sh
## ~/.xinitrc > m@colibri

## merge in defaults and keymaps:
xrdb -merge "/etc/X11/xinit/.Xresources"
xrdb -merge "$HOME/.Xresources"

## start some deamons:
urxvtd -q -f -o
udiskie & # dont forget the &
# thunar --daemon # bugs around!

## start systray programs, dont forget to fork with '&':
pasystray & # volume tray.
nm-applet & # network-manager applet.
blueman-applet & # bluetooth.
cbatticon -r 10 -c 'notify-send Power on 10%' & ## power tray icon

## start keyring:
#eval $(gnome-keyring-daemon --start)
#export SSH_AUTH_SOCK
## ssh managment is moving to grc:
export SSH_AUTH_SOCK=$XDG_RUNTIME_DIR/gcr/ssh

## has to be called before owncloud, via zshrc is not enough!?
export XDG_CONFIG_HOME=/home/m/.config
export QT_QPA_PLATFORMTHEME=Adwaita-Dark
export QT_STYLE_OVERRIDE=Adwaita-Dark

## start cloud clients:
#QT_QPA_PLATFORMTHEME=Adwaita-Dark QT_STYLE_OVERRIDE=Adwaita-Dark nextcloud &
#QT_QPA_PLATFORMTHEME=Adwaita-Dark QT_STYLE_OVERRIDE=Adwaita-Dark owncloud &
nextcloud --style=Adwaita-Dark &
owncloud --style=Adwaita-Dark &

## disable use of header bars in evolution:
gsettings set org.gnome.evolution.shell use-header-bar false
gsettings set org.gnome.desktop.interface color-scheme prefer-dark

## fire up mouse button to key stroke binder for lf/ranger:
xbindkeys

## fire up conky:
#conky -c ~/.config/conky/left.lua
#conky -c ~/.config/conky/middle.lua
#conky -c ~/.config/conky/right.lua

## switch to custom keyboard layout:
## moved to .zshrc otherwise might not be loaded due to latency!
## enables multiple layouts, switch via awesome, us(m) is custom (see notes).
## also just drop the caps lock key, it is a second ctrl now.
#setxkbmap -layout "us(m),de" -option ctrl:nocaps

## composite manager to avoid screen tearing:
picom -b

## turn off powersaver/screensaver/blanking/bell:
xset -dpms s off s noblank -b

## start awesome:
exec awesome

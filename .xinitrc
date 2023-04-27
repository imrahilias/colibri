#!/bin/sh
## ~/.xinitrc > m@colibri

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

## merge in defaults and keymaps
if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi
if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi
if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi
if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

## start some nice programs:
if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

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
eval $(gnome-keyring-daemon --start)
export SSH_AUTH_SOCK

## start cloud clients:
export QT_QPA_PLATFORMTHEME=Adwaita-Dark # has to be called before owncloud, via zshrc is not enough!?
export QT_STYLE_OVERRIDE=Adwaita-Dark
nextcloud &
export QT_QPA_PLATFORMTHEME=Adwaita-Dark # has to be called before owncloud, via zshrc is not enough!?
export QT_STYLE_OVERRIDE=Adwaita-Dark
owncloud &

## disable use of header bars in evolution:
gsettings set org.gnome.evolution.shell use-header-bar false

## fire up mouse button to key stroke binder for ranger:
xbindkeys

## fire up conky:
#conky -c ~/.config/conky/left.lua
#conky -c ~/.config/conky/middle.lua
#conky -c ~/.config/conky/right.lua

## switch to custom keyboard layout:
setxkbmap -layout "us(m),de" # enables multiple layouts, switch via awesome, us(m) is custom (see notes)

## composite manager to avoid screen tearing:
picom -b

## start awesome:
exec awesome

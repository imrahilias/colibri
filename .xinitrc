#!/bin/sh
# ~/.xinitrc > m@colibri

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps
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

# start some nice programs
if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

# start keyring:
#eval $(gnome-keyring-daemon --start)
#export SSH_AUTH_SOCK
# ssh managment is moving to grc:
export SSH_AUTH_SOCK=$XDG_RUNTIME_DIR/gcr/ssh

# has to be called before owncloud, via zshrc is not enough!?
export XDG_CONFIG_HOME="/home/m/.config"

# qt themes use the gtk2/3 theme arc-blackest converted via qt6gtk2:
export QT_QPA_PLATFORMTHEME="gtk2" # qt looks like current gtk theme
export QT_STYLE_OVERRIDE="gtk2"
export ALSA_CARD="PCH"
export ALSA_CTL="PCH"
export CALIBRE_USE_DARK_PALETTE=1
export XDG_CURRENT_DESKTOP="GNOME"

# on arch linux, you should use ~/.xsession instead of ~/.bashrc for
# the csds to be disabled properly:
export GTK_CSD=0
export LD_PRELOAD="/usr/lib/libgtk3-nocsd.so.0${LD_PRELOAD:+:$LD_PRELOAD}"

# start some deamons:
urxvtd -q -f -o
udiskie & # dont forget the &
# thunar --daemon # bugs around!

# start systray programs, dont forget to fork with '&':
pasystray & # volume tray.
nm-applet & # network-manager applet.
blueman-applet & # bluetooth.
cbatticon -r 10 -c 'notify-send Power on 10%' & # volume tray.
1password &

# both *cloud runs on qt, which runs qt6gtk2. so whenever qt or gtk
# change, this qt6gtk2 aur pkg needs to be rebuilt (install, no clean
# build). Also nextcloud might not like the gtk2 theme, so needs to
# run default:
QT_QPA_PLATFORMTHEME='' QT_STYLE_OVERRIDE='' nextcloud &
owncloud &
#syncthingtray &

# disable use of header bars in evolution and other gnome theme stuff:
gsettings set org.gnome.evolution.shell use-header-bar false
gsettings set org.gnome.desktop.interface color-scheme prefer-dark

# fire up mouse button to key stroke binder for lf/ranger:
xbindkeys

# fire up conky:
#conky -c ~/.config/conky/left.lua
#conky -c ~/.config/conky/middle.lua
#conky -c ~/.config/conky/right.lua

# switch to custom keyboard layout:
# moved to .zshrc otherwise might not be loaded due to latency!
# enables multiple layouts, switch via awesome, us(m) is custom (see notes).
# also just drop the caps lock key, it is a second ctrl now.
#setxkbmap -layout "us(m),de" -option ctrl:nocaps

# composite manager to avoid screen tearing:
picom -b

# turn off powersaver/screensaver/blanking/bell:
#xset -dpms
#xset s off
#xset s noblank
xset -b

export XSECURELOCK_AUTH="auth_x11"
export XSECURELOCK_AUTHPROTO="authproto_pam"
export XSECURELOCK_PAM_SERVICE="login"
export XSECURELOCK_BLANK_DPMS_STATE="suspend"
export XSECURELOCK_PASSWORD_PROMPT="time_hex"
export XSECURELOCK_NO_PAM_RHOST=1
export XSECURELOCK_SHOW_HOSTNAME=0
export XSECURELOCK_SHOW_USERNAME=0
export XSECURELOCK_SHOW_KEYBOARD_LAYOUT=0

# start dimmer after 300s and lock after 10 more s:
xset s 300 10
xss-lock -n /usr/lib/xsecurelock/dimmer -l -- xsecurelock &

# start awesome:
exec awesome

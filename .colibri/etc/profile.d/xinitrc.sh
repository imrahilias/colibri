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

# disable use of header bars in evolution and other gnome theme stuff:
gsettings set org.gnome.evolution.shell use-header-bar false
gsettings set org.gnome.desktop.interface color-scheme prefer-dark

export XSECURELOCK_AUTH="auth_x11"
export XSECURELOCK_AUTHPROTO="authproto_pam"
export XSECURELOCK_PAM_SERVICE="login"
export XSECURELOCK_BLANK_DPMS_STATE="suspend"
export XSECURELOCK_PASSWORD_PROMPT="time_hex"
export XSECURELOCK_NO_PAM_RHOST=1
export XSECURELOCK_SHOW_HOSTNAME=0
export XSECURELOCK_SHOW_USERNAME=0
export XSECURELOCK_SHOW_KEYBOARD_LAYOUT=0

# make urxvt work on vsc:
#export TERM=rxvt-unicode
export TERM=rxvt-unicode-256color

# start awesome, thats done by gdm now.
#exec awesome

#!/bin/false
#
#   _ \ __ \\ \   /
#   __/ |   |\ \ /
# \___|_|  _| \_/
#

export EDITOR='emacs'

export GOPATH="$HOME/.go"

# signal now fails.
#export SIGNAL_PASSWORD_STORE='gnome-libsecret'

# 1password unlock.
#export OP_BIOMETRIC_UNLOCK_ENABLED=true

# The `gutter` should by default be `bg+` which it might take the default
# foreground, which is black, and not what i want, so it gets white. Other nice
# options: "--preview-border=none --layout=reverse"
export FZF_DEFAULT_OPTS="--style=minimal --no-height --no-info --no-separator --border=none --prompt '▶ ' --marker='█' --pointer='◆' --color=fg:-1,fg+:-1,bg:-1,bg+:-1,preview-fg:-1,preview-bg:-1,hl:cyan,hl+:magenta,info:-1,marker:magenta,prompt:magenta,spinner:#330099,pointer:magenta,header:-1,border:-1,label:-1,query:-1,gutter:white"
export FZF_ALT_C_OPTS="--preview 'tree -C {} | head -100'"

# Ripgrep will not look in any predetermined directory for a config file
# automatically.
export RIPGREP_CONFIG_PATH="$HOME/.config/ripgreprc"

# qt looks like current gtk theme.
# qt themes use the gtk2/3 theme arc-BLACKEST converted via qt6gtk2,
# has to be run from yay whenever either qt6 or gtk2/3 change,
# no way to do that for two themes in the background though.
#export QT_QPA_PLATFORMTHEME="gtk2" # qt looks like current gtk theme
#export QT_STYLE_OVERRIDE="gtk2"

# ssh managment is moving to grc.
export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/gcr/ssh"

# Has to be called before owncloud.
export XDG_CONFIG_DIRS="/etc/xdg/"
export XDG_CURRENT_DESKTOP="GNOME"

# Default directories.
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DESKTOP_DIR="$HOME"
export XDG_DOCUMENTS_DIR="$HOME/projects"
export XDG_DOWNLOAD_DIR="$HOME/downloads"
export XDG_MUSIC_DIR="$HOME"
export XDG_PICTURES_DIR="$HOME/photos"
export XDG_PUBLICSHARE_DIR="$HOME"
export XDG_TEMPLATES_DIR="$HOME"
export XDG_VIDEOS_DIR="$HOME/movies"

# on arch linux, you should use ~/.xsession instead of ~/.bashrc for
# the csds to be disabled properly:
export GTK_CSD=0
export LD_PRELOAD="/usr/lib/libgtk3-nocsd.so.0${LD_PRELOAD:+:$LD_PRELOAD}"

# Import api keys for datalab.
. "/home/m/asc/bin/api.conf"

export XSECURELOCK_AUTH="auth_x11"
export XSECURELOCK_AUTHPROTO="authproto_pam"
export XSECURELOCK_PAM_SERVICE="xsecurelock"
export XSECURELOCK_BLANK_DPMS_STATE="suspend"
export XSECURELOCK_PASSWORD_PROMPT="time_hex"
export XSECURELOCK_NO_PAM_RHOST=1
export XSECURELOCK_SHOW_HOSTNAME=0
export XSECURELOCK_SHOW_USERNAME=0
export XSECURELOCK_SHOW_KEYBOARD_LAYOUT=0

# Needed for URXVTC on vsc4/5, but not on MUSICA, so it moves to `vsc/.bashrc`.
#export TERM=rxvt-unicode-256color

# If not using the according XDG Desktop Portal, you might have to set the
# environment variable ADW_DISABLE_PORTAL=1 for theme set via GSettings to be
# picked up.
export ADW_DISABLE_PORTAL=1

export GTK_USE_PORTAL=1
export GDK_DEBUG=portals

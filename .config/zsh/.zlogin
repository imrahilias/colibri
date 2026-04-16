#!/bin/false

# .zprofile is for login shells. It is basically the same as .zlogin except that
# it's sourced before .zshrc whereas .zlogin is sourced after .zshrc. According
# to the zsh documentation, ".zprofile is meant as an alternative to .zlogin for
# ksh fans; the two are not intended to be used together, although this could
# certainly be done if desired."

# Disable use of header bars in evolution and other gnome theme stuff.
#gsettings set org.gnome.evolution.shell use-header-bar false
gsettings set org.gnome.desktop.interface color-scheme prefer-light
gsettings set org.gnome.desktop.interface gtk-theme Arc
gsettings set org.gnome.desktop.interface enable-animations false

# 12 is all the switchable workspaces gnome can have.
gsettings set org.gnome.mutter dynamic-workspaces false
gsettings set org.gnome.desktop.wm.preferences num-workspaces 12
gsettings set "org.gnome.desktop.wm.keybindings" "switch-to-workspace-1" "['<Super>grave']"
gsettings set "org.gnome.desktop.wm.keybindings" "move-to-workspace-1" "['<Super><Shift>grave']"
for i in {2..9}; do
    gsettings set "org.gnome.shell.keybindings" "switch-to-application-${i}" "[]"
    gsettings set "org.gnome.desktop.wm.keybindings" "switch-to-workspace-${i}" "['<Super>$((i-1))']"
    gsettings set "org.gnome.desktop.wm.keybindings" "move-to-workspace-${i}" "['<Super><Shift>$((i-1))']"
done
gsettings set "org.gnome.desktop.wm.keybindings" "switch-to-workspace-10" "['<Super>9']"
gsettings set "org.gnome.desktop.wm.keybindings" "move-to-workspace-10" "['<Super><Shift>9']"
gsettings set "org.gnome.desktop.wm.keybindings" "switch-to-workspace-11" "['<Super>0']"
gsettings set "org.gnome.desktop.wm.keybindings" "move-to-workspace-11" "['<Super><Shift>0']"


# You can replace the -eq comparison with one like -le 3 (for vt1 to vt3) if you
# want to use graphical logins on more than one virtual terminal. The `exec
# startx` command ensures that the user is logged out when the X server exits,
# crashes or is killed by an attacker. If you want to take the risk and remain
# logged in when the X session ends, remove exec.

# if [ -z "${DISPLAY}" ] && [ "${XDG_VTNR}" -le 3 ]; then
#   exec startx
# fi

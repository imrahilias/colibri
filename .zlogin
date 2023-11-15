#!/bin/false
# Autostart X at login. .zprofile is for login shells. It is basically
# the same as .zlogin except that it's sourced before .zshrc whereas
# .zlogin is sourced after .zshrc. According to the zsh documentation,
# ".zprofile is meant as an alternative to .zlogin for ksh fans; the
# two are not intended to be used together, although this could
# certainly be done if desired." You can replace the -eq comparison
# with one like -le 3 (for vt1 to vt3) if you want to use graphical
# logins on more than one virtual terminal. The `exec startx` command
# ensures that the user is logged out when the X server exits, crashes
# or is killed by an attacker. If you want to take the risk and remain
# logged in when the X session ends, remove exec.

if [ -z "${DISPLAY}" ] && [ "${XDG_VTNR}" -le 3 ]; then
  startx
fi

#!/bin/false
# Autostart X at login. You can replace the -eq comparison with one
# like -le 3 (for vt1 to vt3) if you want to use graphical logins on
# more than one virtual terminal. The exec command ensures that the
# user is logged out when the X server exits, crashes or is killed by
# an attacker. If you want to take the risk and remain logged in when
# the X session ends, remove exec.

if [ -z "${DISPLAY}" ] && [ "${XDG_VTNR}" -le 3 ]; then
  exec startx
fi

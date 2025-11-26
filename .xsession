# on arch linux, you should use ~/.xsession instead of ~/.bashrc for
# the csds to be disabled properly:
export GTK_CSD=0
export LD_PRELOAD="/usr/lib/libgtk3-nocsd.so.0${LD_PRELOAD:+:$LD_PRELOAD}"
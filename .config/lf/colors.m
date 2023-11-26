# vim:ft=dircolors
# (This is not a dircolors file but it helps to highlight colors and comments)
#
#                 |           
#   __|  _ \   _` |  _ \  __| 
#  (    (   | (   |  __/\__ \ 
# \___|\___/ \__,_|\___|____/ 
#
# Below are the color init strings for the basic file types. A color init
# string consists of one or more of the following numeric codes:
#
# Attribute codes:
#   00=none 01=bold 04=underscore 05=blink 07=reverse 08=concealed
# Text color codes:
#   30=black 31=red 32=green 33=yellow 34=blue 35=magenta 36=cyan 37=white
# Background color codes:
#   40=black 41=red 42=green 43=yellow 44=blue 45=magenta 46=cyan 47=white
#
# Example:
# .out 01;31
# .out 01;41
#
# 8-bit / 256color:
#
# BREAKS RANGER, but works in urxvt!
#
# As 256-color lookup tables became common on graphic cards, escape
# sequences were added to select from a pre-defined set of 256 colors:
#
# ESC[38;5;⟨n⟩m Select foreground color      where n is a number from the table below
# ESC[48;5;⟨n⟩m Select background color
#   0-  7:  standard colors (as in ESC [ 30–37 m)
#   8- 15:  high intensity colors (as in ESC [ 90–97 m)
#  16-231:  6 × 6 × 6 cube (216 colors): 16 + 36 × r + 6 × g + b (0 ≤ r, g, b ≤ 5)
# 232-255:  grayscale from dark to light in 24 steps
#
# Example:
# .out 38;5;161
# .out 48;5;161
#
# #  |                    
# #  __| |   | __ \   _ \ 
# #  |   |   | |   |  __/ 
# # \__|\__, | .__/ \___| 
# #     ____/ _|          
#
# NORMAL 00 # no color code at all
# FILE 00 # regular file: use no color at all
# RESET 0 # reset to "normal" color
# DIR 01;34 # directory
# ## Link 'target' breaks ranger!
# ## If you set this to 'target' instead of a numerical value,
# ## the color is as for the file pointed to.
# LINK 01;36 # symbolic link. 
# MULTIHARDLINK 00 # regular file with more than one link
# FIFO 30;47 # pipe
# SOCK 01;45 # socket
# DOOR 45 # door
# BLK 42 # block device driver      
# CHR 01;42 # character device driver
#
# #        |   |       _) |           |        
# #   _` | __| __|  __| | __ \  |   | __|  _ \ 
# #  (   | |   |   |    | |   | |   | |    __/ 
# # \__,_|\__|\__|_|   _|_.__/ \__,_|\__|\___| 
# #
# ORPHAN 01;36 # symlink to nonexistent file, or non-stat'able file ...
# MISSING 01;31 # ... and the files that orphaned symlinks point to (blinking white on red)
# SETUID 01;41 # file that is setuid (u+s)
# SETGID 41 # file that is setgid (g+s)
# CAPABILITY 01;44 # file with capability
# STICKY_OTHER_WRITABLE 01;30;43 # dir that is sticky and other-writable (+t,o+w)
# OTHER_WRITABLE 30;43 # dir that is other-writable (o+w) and not sticky
# STICKY 30;43 # dir with the sticky bit set (+t) and not other-writable
# EXEC 01 # This is for files with execute permission (+x)


#
#  |  _| 
#  | |   
#  | __| 
# _|_|   

# default values from dircolors
# (entries with a leading # are not implemented in lf)
# #no     00              # NORMAL
# fi      00              # FILE
# #rs     0               # RESET
# di      01;34           # DIR
# ln      01;36           # LINK
# #mh     00              # MULTIHARDLINK
# pi      40;33           # FIFO
# so      01;35           # SOCK
# #do     01;35           # DOOR
# bd      40;33;01        # BLK
# cd      40;33;01        # CHR
# or      40;31;01        # ORPHAN
# #mi     00              # MISSING
# su      37;41           # SETUID
# sg      30;43           # SETGID
# #ca     30;41           # CAPABILITY
# tw      30;42           # STICKY_OTHER_WRITABLE
# ow      34;42           # OTHER_WRITABLE
# st      37;44           # STICKY
# ex      01;32           # EXEC

# default values from lf (with matching order)
# ln      01;36   # LINK
# or      31;01   # ORPHAN
# tw      01;34   # STICKY_OTHER_WRITABLE
# ow      01;34   # OTHER_WRITABLE
# st      01;34   # STICKY
# di      01;34   # DIR
# pi      33      # FIFO
# so      01;35   # SOCK
# bd      33;01   # BLK
# cd      33;01   # CHR
# su      01;32   # SETUID
# sg      01;32   # SETGID
# ex      01;32   # EXEC
# fi      00      # FILE

# file types (with matching order)
ln      01;36      # LINK
or      31;01      # ORPHAN
tw      01;30;43   # STICKY_OTHER_WRITABLE
ow      30;43      # OTHER_WRITABLE
st      30;43      # STICKY
di      01;34      # DIR
pi      30;47      # FIFO
so      01;45      # SOCK
bd      42         # BLK
cd      01;42      # CHR
su      01;41      # SETUID
sg      41         # SETGID
ex      01         # EXEC
fi      00         # FILE

#             |   
#   _ \\ \  / __| 
#   __/ `  <  |   
# \___| _/\_\\__| 
#                
# List any file extensions like '.gz' or '.tar' that you would like ls
# to colorize below. Put the extension, a space, and the color init string.
# (and any comments you want to add after a '#')

## Text that we can edit with a regular editor:
*.txt 00

## Source text:
*.0 33
*.1 33
*.2 33
*.3 33
*.4 33
*.5 33
*.6 33
*.7 33
*.8 33
*.9 33
*.C 33
*.bash 33
*.c 33
*.cc 33
*.cl 33
*.coffee 33
*.cpp 33
*.csh 33
*.css 33
*.csv 33
*.cxx 33
*.el 33
*.erb 33
*.f 33
*.f90 33
*.go 33
*.gp 33
*.h 33
*.haml 33
*.hpp 33
*.hs 33
*.htm 33
*.html 33
*.ipynb 33
*.java 33
*.js 33
*.l 33
*.less 33
*.m 33
*.man 33
*.md 33
*.mkd 33
*.n 33
*.objc 33
*.org 33
*.p 33
*.php 33
*.pl 33
*.pm 33
*.pod 33
*.py 33
*.rb 33
*.rdf 33
*.sass 33
*.scss 33
*.sh 33
*.shtml 33
*.sql 33
*.sv 33
*.svh 33
*.tex 33
*.v 33
*.vh 33
*.vhd 33
*.vim 33
*.xml 33
*.zsh 33

# Image:
*.JPG 92
*.PNG 92
*.bmp 92
*.cgm 92
*.dl 92
*.dvi 92
*.emf 92
*.eps 92
*.gif 92
*.jpeg 92
*.jpg 92
*.mng 92
*.pbm 92
*.pcx 92
*.pgm 92
*.png 92
*.ppm 92
*.pps 92
*.ppsx 92
*.ps 92
*.svg 92
*.svgz 92
*.tga 92
*.tif 92
*.tiff 92
*.xbm 92
*.xcf 92
*.xpm 92
*.xwd 92
*.xwd 92
*.yuv 92

## Audio:
*.aac 94
*.au  94
*.flac 94
*.m4a 94
*.mid 94
*.midi 94
*.mka 94
*.mp3 94
*.mpa 94
*.mpeg 94
*.mpg 94
*.ogg  94
*.opus 94
*.ra 94
*.wav 94

## Video:
*.MOV 35
*.anx 35
*.asf 35
*.avi 35
*.axv 35
*.flc 35
*.fli 35
*.flv 35
*.gl 35
*.m2v 35
*.m4v 35
*.mkv 35
*.mov 35
*.mp4 35
*.mp4v 35
*.mpeg 35
*.mpg 35
*.nuv 35
*.ogm 35
*.ogv 35
*.ogx 35
*.qt 35
*.rm 35
*.rmvb 35
*.swf 35
*.vob 35
*.webm 35
*.wmv 35

## Binary document formats and multimedia source:
*.doc 32
*.docx 32
*.dot 32
*.dotx 32
*.fla 32
*.odp 32
*.ods 32
*.odt 32
*.otp 32
*.ots 32
*.ott 32
*.pdf 32
*.ppt 32
*.pptx 32
*.psd 32
*.rtf 32
*.xls 32
*.xlsx 32

## Archives, compressed
*.7z   01;35
*.Z    01;35
*.apk  01;35
*.arj  01;35
*.bin  01;35
*.bz   01;35
*.bz2  01;35
*.cab  01;35  # Win
*.deb  01;35
*.dmg  01;35  # OSX
*.gem  01;35
*.gz   01;35
*.iso  01;35
*.jar  01;35
*.msi  01;35  # Win
*.rar  01;35
*.rpm  01;35
*.tar  01;35
*.tbz  01;35
*.tbz2 01;35
*.tgz  01;35
*.tx   01;35
*.war  01;35
*.xpi  01;35
*.xz   01;35
*.z    01;35
*.zip  01;35

## Unimportant text files:
*# 90
*~ 90
*.log 90

## Unimportant non-text files:
*,v 01;90
*.BAK 01;90
*.DIST 01;90
*.OFF 01;90
*.OLD 01;90
*.ORIG 01;90
*.bak 01;90
*.dist 01;90
*.off 01;90
*.old 01;90
*.org_archive 01;90
*.orig 01;90
*.swo 01;90
*.swp 01;90

## Custom file type:
*.3des 34
*.aes 34
*.asc 34
*.enc 34
*.gpg 34
*.gpg 34
*.pgp 34
*.sqlite 34

## Other files with execute permission:
*.app 31  # OSX
*.bat 31  # Win
*.cmd 31  # Win
*.com 31  # Win
*.exe 31  # Win
*.reg 31  # Win
*.out 01

## Only enable for testing (zsh function /bin/colordemo):
# *.3000 30
# *.3001 01;30
# *.3100 31
# *.3101 01;31
# *.3200 32
# *.3201 01;32
# *.3300 33
# *.3301 01;33
# *.3400 34
# *.3401 01;34
# *.3500 35
# *.3501 01;35
# *.3600 36
# *.3601 01;36
# *.3700 37
# *.3701 01;37
# *.9000 90
# *.9001 01;90
# *.9100 91
# *.9101 01;91
# *.9200 92
# *.9201 01;92
# *.9300 93
# *.9301 01;93
# *.9400 94
# *.9401 01;94
# *.9500 95
# *.9501 01;95
# *.9600 96
# *.9601 01;96
# *.9700 97
# *.9701 01;97
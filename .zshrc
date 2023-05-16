#!/bin/false
#blabla

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# The PS1 variable may be unset or empty and the shell may still be
# interactive (but without a prompt).
for i in "$-"
do
    case "$-" in *i*) ;;
                  *) return ;;
    esac
done

#        |_)            
#   _` | | |  _` |  __| 
#  (   | | | (   |\__ \ 
# \__,_|_|_|\__,_|____/ 

if [[ $EUID != 0 ]] ; then
    source /home/m/vscloud/bin/aliases
fi

## colors
red="\e[31m"
blue="\e[34m"
default="\e[0m"

## enable color support of ls and also add handy aliases.
if [ -x /usr/bin/dircolors ]
then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias -g ls='ls --color=auto'
    alias -g dir='dir --color=auto'
    alias -g vdir='vdir --color=auto'

    alias -g grep='grep --ignore-case --color=auto'
    alias -g fgrep='fgrep --ignore-case --color=auto'
    alias -g egrep='egrep --ignore-case --color=auto'

    alias -g diff='diff --color=always'
    alias -g less='less -r'
    #alias ll='less -r'
fi

## list aliasas
alias l='ls -1Bhl --group-directories-first --color=always' # '1' 4 one entry/line, 'B' ignores backups (~), 'h' 4 human readable (kiB, MiB, ...).
alias ll='ls -1ABhl --group-directories-first --color=always' # 'A' 4 almost all.
alias d='dirs -v' # lists zsh directory stack (enter <cd +- tab>, plus & minus (reverse) literally, with completion!.
alias blk='sudo blkid -o list'
alias hist='fc -El 0 | grep'
alias lsa='lsarchives '

## edit aliases
alias ef='sudo cp /etc/fstab /etc/fstab.$(date +%y%m%d%H%S) && sudo emacs /etc/fstab'
alias eb='emacsclient -c $HOME/.bashrc'
alias ez='emacsclient -c $HOME/.zshrc'
alias ex='emacsclient -c $HOME/.xinitrc'
alias ea='emacsclient -c $HOME/.config/awesome/rc.lua'

## mount aliases
alias mnta='sudo mount -a; echo -e $red"mounted:"$default; mount' # echo 4 color, semicolon 4 1. command, if ok, than 2. com.
alias uma='sudo umount -a; echo -e $red"mounted:"$default; mount'

## pacman aliases
alias pii='sudo pacman -S' # install 1 pkg.
alias pi='sudo pacman -Syyu' # do a full system upgrade.
alias pq='bauerbill -Ss --aur' # search for all repo and AUR packages.
alias pp='sudo powerpill -Syyu' # do a full system upgrade using pauerpill with rsync.
alias ppa='sudo bb-wrapper -Syyu --aur' # do a full system upgrade with AUR support using bauerbill with rsync.
alias pa='sudo bb-wrapper -Su --aur' # install from AUR using bauerbill with rsync.
alias px='sudo pacman -R' # remove package.
alias pc='sudo pacman -Scc && sudo pacman-optimize' # remove all cached pkg! and defragment.
alias reflect='sudo reflector -p https -f 10 -l 10 --sort rate --save /etc/pacman.d/mirrorlist' # save 10 fastest of the 10 recent mirrors using https.

## file aliases
alias duh='du -d 1 -h' # display the size of files at depth 1 in current location in human-readable form.
alias df='df -h'
alias countf='find . -type f | wc -l' # number of all files in dir.
alias countd='find . -type d | wc -l' # number of all subdirs in dir.
#alias rmtmp='rm *\#; rm *~; rm .*~' # moved to script.
alias xx='extract '

## network alias
alias oe1='mplayer http://mp3stream3.apasf.apa.at:8000/listen.pls'

## launch alias
alias x='startx'
#alias evince='dbus-launch evince'
alias e="emacsclient -ca \'\'" # > service moved to systemd
alias scan='scanimage --format=tiff --mode=Color' #>http://lists.alioth.debian.org/pipermail/sane-devel/2001-December/001177.html
alias am='alsamixer'
alias halt='systemctl poweroff'
alias sus='systemctl suspend'
#alias sus='umount /mnt/vsc3; umount /mnt/vsc4; sleep 10; systemctl suspend'
alias hib='systemctl hibernate'

## misc
alias s='sudo su -'
alias c='clear'
alias u='urxvtc'
alias rdark='razercfg -l all:off'
alias rlight='razercfg -l GlowingLogo:off -l Scrollwheel:on'
alias rfast='razercfg -r 1:3'
alias rslow='razercfg -r 1:2'
alias rename='perl-rename'
#alias zephyr='/usr/bin/git --git-dir=$HOME/.zephyr --work-tree=$HOME'
alias rainbow='for (( i = 30; i < 38; i++ )); do echo -e "\033[0;"$i"m Normal: (0;$i); \033[1;"$i"m Light: (1;$i)"; done'
alias fifi='figlet -w 200 -f "shadow" '
alias cp='rsync -aP' # show percentage.
#alias ap='adb pull /sdcard/DCIM/Camera/ /mnt/troika/photos/2021/' more complicated: moved to bin.
alias d0='xrandr --auto'
alias d1='xrandr --output HDMI-A-0 --auto --output eDP --off'
alias d2='xrandr --output HDMI-A-0 --auto --right-of eDP'
alias d3='xrandr --output DisplayPort-1 --scale 0.8 --output eDP --off'
alias pm='pulsemixer'
alias wh='which '
alias r='zranger'
alias scrot='scrot ~/.screens/%H%M%S.png'
alias p='python3 '

#              |  _)                  
#   _ \  __ \  __| |  _ \  __ \   __| 
#  (   | |   | |   | (   | |   |\__ \ 
# \___/  .__/ \__|_|\___/ _|  _|____/ 
#       _|                            

setopt extendedglob             # inverted expansion like: ls *~*.txt.
#setopt correct                  # correct mistakes.
setopt auto_list                # list choice on ambiguous command.
setopt listtypes                # %1 killed. will show up exactly when it is killed.
setopt auto_cd                  # change dir by just typing its name wo cd.
setopt auto_pushd               # automatically adds dirs to stack.
setopt prompt_subst             # prompt more dynamic, allow function in prompt.
setopt no_beep                  # never ever beep ever (alt: unsetopt beep).
setopt rm_star_wait             # Wait, and ask if the user is serious when doing rm *.
#setopt completealiases          # is enabled elsewhere/ otherwise no effect.
setopt append_history           # Don't overwrite history.
#setopt inc_append_history       # saves in chronological order, all sessions.
setopt share_history            # even more, sessioins share the same file!
setopt hist_ignore_all_dups     # when runing a command several times, only store one.
setopt hist_reduce_blanks       # reduce whitespace in history.
setopt hist_ignore_space        # do not remember commands starting with space.
HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000

#   _|                  |  _)                  
#  |   |   | __ \   __| __| |  _ \  __ \   __| 
#  __| |   | |   | (    |   | (   | |   |\__ \ 
# _|  \__,_|_|  _|\___|\__|_|\___/ _|  _|____/ 

## archive extraction
extract() {
    local remove_archive
    local success
    local extract_dir

    if (( $# == 0 ))
    then
        cat <<-'EOF' >&2
			Usage: extract [-option] [file ...]
			Options:
			    -r, --remove    Remove archive after unpacking.
		EOF
    fi

    remove_archive=1
    if [[ "$1" == "-r" ]] || [[ "$1" == "--remove" ]]
    then
	remove_archive=0
	shift
    fi

    while (( $# > 0 ))
    do
	if [[ ! -f "$1" ]]
        then
	    echo "extract: '$1' is not a valid file" >&2
	    shift
	    continue
	fi

	success=0
	extract_dir="${1:t:r}"
	case "${1:l}" in
	    (*.tar.gz|*.tgz) (( $+commands[pigz] )) && { pigz -dc "$1" | tar xv } || tar zxvf "$1" ;;
	    (*.tar.bz2|*.tbz|*.tbz2) tar xvjf "$1" ;;
	    (*.tar.xz|*.txz)
		tar --xz --help &> /dev/null \
		    && tar --xz -xvf "$1" \
			|| xzcat "$1" | tar xvf - ;;
	    (*.tar.zma|*.tlz)
		tar --lzma --help &> /dev/null \
		    && tar --lzma -xvf "$1" \
			|| lzcat "$1" | tar xvf - ;;
	    (*.tar) tar xvf "$1" ;;
	    (*.gz) (( $+commands[pigz] )) && pigz -d "$1" || gunzip "$1" ;;
	    (*.bz2) bunzip2 "$1" ;;
	    (*.xz) unxz "$1" ;;
	    (*.lzma) unlzma "$1" ;;
	    (*.z) uncompress "$1" ;;
	    (*.zip|*.war|*.jar|*.sublime-package|*.ipsw|*.xpi|*.apk|*.aar|*.whl) unzip "$1" -d $extract_dir ;;
	    (*.rar) unrar x -ad "$1" ;;
	    (*.7z) 7za x "$1" ;;
	    (*.deb)
		mkdir -p "$extract_dir/control"
		mkdir -p "$extract_dir/data"
		cd "$extract_dir"; ar vx "../${1}" > /dev/null
		cd control; tar xzvf ../control.tar.gz
		cd ../data; extract ../data.tar.*
		cd ..; rm *.tar.* debian-binary
		cd ..
		;;
	    (*)
		echo "extract: '$1' cannot be extracted" >&2
		success=1
		;;
	esac

	(( success = $success > 0 ? $success : $? ))
	(( $success == 0 )) && (( $remove_archive == 0 )) && rm "$1"
	shift
    done
}

## list archives
lsarchive() {
    if [ -f $1 ]
    then
        case $1 in
            *.tar.bz2)      tar jtf $1      ;;
            *.tar.gz)       tar ztf $1      ;;
            *.tar)          tar tf $1       ;;
            *.tgz)          tar ztf $1      ;;
            *.zip)          unzip -l $1     ;;
            *.rar)          rar vb $1       ;;
            *.7z)           7z l $1         ;;
            *)              echo"'$1' Error. I have no idea what to do with that";;
        esac
    else
        echo "'$1' is not a valid archive"
    fi
}

## man coloring
man() {
    env LESS_TERMCAP_mb=$'\E[01;31m' \
        LESS_TERMCAP_md=$'\E[01;38;5;74m' \
        LESS_TERMCAP_me=$'\E[0m' \
        LESS_TERMCAP_se=$'\E[0m' \
        LESS_TERMCAP_so=$'\E[38;5;246m' \
        LESS_TERMCAP_ue=$'\E[0m' \
        LESS_TERMCAP_us=$'\E[04;38;5;146m' \
        man "$@"
}


## Pressing enter in a git directory runs `git status`
## in other directories `ls`.
magic-enter () {

    ## If commands are not already set, use the defaults.
    [ -z "$MAGIC_ENTER_GIT_COMMAND" ] && MAGIC_ENTER_GIT_COMMAND="git status ."
    [ -z "$MAGIC_ENTER_OTHER_COMMAND" ] && MAGIC_ENTER_OTHER_COMMAND="ls -1Bhl --group-directories-first ."

    if [[ -z $BUFFER ]]
    then
        echo ""
        if git rev-parse --is-inside-work-tree &>/dev/null
        then
            eval "$MAGIC_ENTER_GIT_COMMAND"
        else
            eval "$MAGIC_ENTER_OTHER_COMMAND"
        fi
        zle redisplay
    else
        zle accept-line
    fi
}
zle -N magic-enter
bindkey "^M" magic-enter


## cd + ls
function chpwd() {
    emulate -L zsh
    ls -1Bhl --group-directories-first --color=auto . # runs ls (...) after typing cd!
}


## anti tar-bomb
atb() {
    l=$(tar tf $1)
    if [ $(echo "$l" | wc -l) -eq $(echo "$l" | grep $(echo "$l" | head -n1) | wc -l) ]
    then
        tar xf $1
    else mkdir ${1%.t(ar.gz||ar.bz2||gz||bz||ar)} && tar xf $1 -C ${1%.t(ar.gz||ar.bz2||gz||bz||ar)}
    fi
}


## quick dir change
rationalize-dot() {
    if [[ $LBUFFER = *.. ]]
    then
        LBUFFER+=/..
    else
        LBUFFER+=.
    fi
}
zle -N rationalize-dot
bindkey . rationalize-dot


## changes the prompt char to 'g' if the current dir is a git repo:
function prompt_char {
    git branch >/dev/null 2>/dev/null && echo ' + ' && return 
    echo ' '
}


# function cmd_fail {
#     if [ "`echo $?`" -ne "0" ]
#     then
# 	echo ":( "
#     fi
# }


## if the current dir is a git repo, it prints the current branch and
## a * if there is stuff to be commited:
function git_branch {
    git branch >/dev/null 2>/dev/null && echo -n "git:"$(git branch | grep "*" | sed 's/* //')
    git status >/dev/null 2>/dev/null | grep modified >/dev/null 2>/dev/null && echo "* " && return
    echo " "
}


# ## sudo or sudoedit will be inserted before the command @ Dongweiming <ciici123@gmail.com>
# sudo-command-line() {
#     [[ -z $BUFFER ]] && zle up-history
#     if [[ $BUFFER == sudo\ * ]]; then
#         LBUFFER="${LBUFFER#sudo }"
#     elif [[ $BUFFER == $EDITOR\ * ]]; then
#         LBUFFER="${LBUFFER#$EDITOR }"
#         LBUFFER="sudoedit $LBUFFER"
#     elif [[ $BUFFER == sudoedit\ * ]]; then
#         LBUFFER="${LBUFFER#sudoedit }"
#         LBUFFER="$EDITOR $LBUFFER"
#     else
#         LBUFFER="sudo $LBUFFER"
#     fi
# }
# zle -N sudo-command-line
# ## Defined shortcut keys: [Esc] [Esc]
# bindkey -M emacs '\e\e' sudo-command-line
# #bindkey -M vicmd '\e\e' sudo-command-line
# #bindkey -M viins '\e\e' sudo-command-line

## colors, a lot of colors!
function clicolors() {
    i=1
    for color in {000..255}
    do
        c=$c"$FG[$color]$color✔$reset_color  "
        if [ `expr $i % 8` -eq 0 ]
        then
            c=$c"\n"
        fi
        i=`expr $i + 1`
    done
    echo $c | sed 's/%//g' | sed 's/{//g' | sed 's/}//g' | sed '$s/..$//'
    c=''
}

#   __| _` | __ \   _` |  _ \  __| 
#  |   (   | |   | (   |  __/ |    
# _|  \__,_|_|  _|\__, |\___|_|    
#                 |___/            

## This is based on: https://github.com/ranger/ranger/blob/master/examples/bash_automatic_cd.sh
## Paste this into your .zshrc:

# function zranger {
#     tempfile="$(mktemp -t tmp.XXXXXX)"
#     /usr/bin/ranger --choosedir="$tempfile" "${@:-$(pwd)}"
#     test -f "$tempfile" &&
#     if [ "$(cat -- "$tempfile")" != "$(echo -n `pwd`)" ]; then
#         cd -- "$(cat "$tempfile")"
#     fi  
#     rm -f -- "$tempfile"
# }

## https://wiki.archlinux.org/index.php/Ranger
#$RANGERCD && unset RANGERCD && ranger-cd

# _  /  __| _` | __ \   _` |  _ \  __| 
#   /  |   (   | |   | (   |  __/ |    
# ___|_|  \__,_|_|  _|\__, |\___|_|    
#                     |___/            

# Preventing nested ranger instances You can start a shell in the
# current directory with S, when you exit the shell you get back to
# your ranger instance. When you however forget that you already are
# in a ranger shell and start ranger again you end up with ranger
# running a shell running ranger.

zranger() {
    if [ -z "$RANGER_LEVEL" ]
    then
        . ranger # start in current dir
    else
        exit
    fi
}

## ranger-cd will fire for Ctrl+D
bindkey -s '^D' 'zranger\n'

### based on https://github.com/Vifon/zranger
## switching retains tabs!
# zranger() {
#     local RANGER_PID

#     if RANGER_PID=$(tmux list-panes -s -F '#{pane_pid}' -t ranger 2> /dev/null); then
#         # Leave the current cwd for ranger to read and cleanup.
#         pwd > /tmp/zranger-cwd-$UID
#         # Detach the other zranger instance...
#         tmux detach-client -s ranger
#         # ...and give it some time to read ranger's cwd before it changes.
#         sleep 0.05              # May need some tweaking.
#         # Tell ranger to read zsh's cwd from /tmp and cd to it.
#         kill -SIGUSR1 $RANGER_PID
#         # Attach to it.
#         TMUX='' tmux attach -t ranger
#     else
#         TMUX='' tmux new-session -s ranger 'exec ranger --cmd="set preview_images=false"'
#     fi

#     # A second check needed because the process could have been
#     # started or stopped in the meantime.
#     if RANGER_PID=$(tmux list-panes -s -F '#{pane_pid}' -t ranger 2> /dev/null); then
#         cd -P /proc/$RANGER_PID/cwd
#     fi
# }

# ## ranger-cd will fire for 'Ctrl+D':
# #autoload -U zranger # embedded in zshrc
# bindkey -s '^D' "\eq zranger\n"

#                                  |   
#  __ \   __| _ \  __ `__ \  __ \  __| 
#  |   | |   (   | |   |   | |   | |   
#  .__/ _|  \___/ _|  _|  _| .__/ \__| 
# _|                        _|         

black="%{"$'\033[00;30m'"%}"
bblack="%{"$'\033[01;30m'"%}"
red="%{"$'\033[00;31m'"%}"
bred="%{"$'\033[01;31m'"%}"
green="%{"$'\033[00;32m'"%}"
bgreen="%{"$'\033[01;32m'"%}"
yellow="%{"$'\033[00;33m'"%}"
byellow="%{"$'\033[01;33m'"%}"
blue="%{"$'\033[00;34m'"%}"
bblue="%{"$'\033[01;34m'"%}"
magenta="%{"$'\033[00;35m'"%}"
bmagenta="%{"$'\033[01;35m'"%}"
cyan="%{"$'\033[00;36m'"%}"
bcyan="%{"$'\033[01;36m'"%}"
white="%{"$'\033[00;37m'"%}"
bwhite="%{"$'\033[01;37m'"%}"
norm="%{"$'\033[00m'"%}"

if [[ $EUID == 0 ]] ; then
    PROMPT="${bred}%~"'$(prompt_char)'"${white}"
else
    PROMPT="${bwhite}%~"'$(prompt_char)'"${white}"
fi
RPROMPT='%D{%H%M}'


#   _ \ __ \\ \   / 
#   __/ |   |\ \ /  
# \___|_|  _| \_/   

export EDITOR='emacsclient -c -a ""'
export PATH='/home/m/bin:/home/m/vscloud/bin:/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin:/usr/local/sbin:/usr/games:/usr/local/games:/opt'
#PATH+=/scripts # hängt zur $path eben was an...
#export QT_QPA_PLATFORMTHEME='qt5ct' # qt5 gtk blending
#export QT_STYLE_OVERRIDE='qt5ct'
#export QT_QPA_PLATFORMTHEME='gtk2' # qt looks like current gtk theme 
#export QT_STYLE_OVERRIDE='gtk2'
QT_QPA_PLATFORMTHEME='Adwaita-Dark'
QT_STYLE_OVERRIDE='Adwaita-Dark'
export ALSA_CARD='PCH'
export ALSA_CTL='PCH'
export CALIBRE_USE_DARK_PALETTE=1
export XDG_CURRENT_DESKTOP='GNOME'

#       |          _|  _| 
#   __| __| |   | |   |   
# \__ \ |   |   | __| __| 
# ____/\__|\__,_|_|  _|

## turn off XOFF/XON
stty -ixon

## turn off powersaver/screensaver/blanking/bell. moved to xinitrc.
#xset -dpms s off s noblank -b

## key setups
bindkey -e # emacs key bindings: yeeha:D
bindkey ' ' magic-space # also do history expansion on space, type '!!', then hit enter, watch.

## word jumping
zle -A delete-char delete-char-num 
zle -A overwrite-mode overwrite-mode-num
bindkey ";5C" forward-word
bindkey ";5D" backward-word
bindkey "[C" forward-word
bindkey "[D" backward-word
bindkey "^[Oc" forward-word
bindkey "^[Od" backward-word
bindkey ";5A" up-line
bindkey ";5B" down-line
bindkey "^[[5~" up-history
bindkey "^[[6~" down-history
bindkey "^[[2~" overwrite-mode
bindkey "^[Op"  overwrite-mode-num
bindkey "${terminfo[khome]}" beginning-of-line
bindkey "${terminfo[kend]}" end-of-line
bindkey "^[[3~" delete-char
bindkey "^[On"  delete-char-num

#        |            _)            
#  __ \  | |   |  _` | | __ \   __| 
#  |   | | |   | (   | | |   |\__ \ 
#  .__/ _|\__,_|\__, |_|_|  _|____/ 
# _|            |___/               

## auto suggestion
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=6,bg=grey"
ZSH_AUTOSUGGEST_STRATEGY=(match_prev_cmd completion) # will first try to find a suggestion from your history, but, if it can't find a match, will find a suggestion from the completion engine (experimental).
ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20
ZSH_AUTOSUGGEST_USE_ASYNC=1

## history-substring-search:
source /usr/share/zsh/plugins/zsh-history-substring-search/zsh-history-substring-search.zsh

## autojump:
#source /etc/profile.d/autojump.zsh

## syntax highlighning has to be last:
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern cursor)

## more colors! > manually copied:
#source ~/.zsh/zsh-dircolors/zsh-dircolors.plugin.zsh 

#                             
#   __|  _ \  __ `__ \  __ \  
#  (    (   | |   |   | |   | 
# \___|\___/ _|  _|  _| .__/  
#                      _|     

autoload -Uz compinit; compinit
autoload -U colors && colors
zstyle ':completion:*' completer _match _expand _complete _correct _approximate
zstyle ':completion:*' completions 1
zstyle ':completion:*' file-sort name
zstyle ':completion:*' glob 1
zstyle ':completion:*' insert-unambiguous true
zstyle ':completion:*' max-errors 2
zstyle ':completion:*' original true
zstyle ':completion:*' substitute 1
zstyle ':completion:*' special-dirs true # tab-completion for .. and others
zstyle ':completion:*:descriptions' format '%U%B%d%b%u'
zstyle ':completion:*' menu select
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' squeeze-slashes true
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.config/shell/zsh_cache
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*:match:*' original only
zstyle ':completion:*' verbose true
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:*:kill:*:processes' command 'ps haxopid:5,user:4,%cpu:4,ni:2,stat:3,etime:8,args'
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:kill:*' force-list always
zstyle ':completion:*:processes' command "ps -au${USER}"
zstyle ':completion:*:sudo:*' command-path /bin /usr/bin /usr/local/bin /home/m/bin /sbin /usr/sbin /usr/local/sbin /usr/games /usr/local/games


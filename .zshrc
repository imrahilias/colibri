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

## sourcing "/home/m/.config/lf" stuff appearently raises "insecure
## directories" for compinit otherwise:
ZSH_DISABLE_COMPFIX="true"

## switch to custom keyboard layout:
## this could be set in .xinitrc but might not be loaded due to latency!
## enables multiple layouts, switch via awesome, us(m) is custom (see notes).
## also just drop the caps lock key, it is a second ctrl now.
setxkbmap -layout "us(m),de" -option ctrl:nocaps


#                                  |
#  __ \   __| _ \  __ `__ \  __ \  __|
#  |   | |   (   | |   |   | |   | |
#  .__/ _|  \___/ _|  _|  _| .__/ \__|
# _|                        _|

autoload -Uz add-zsh-hook vcs_info
add-zsh-hook precmd vcs_info
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:git:*' check-for-changes true
zstyle ':vcs_info:git:*' check-for-staged-changes true
zstyle ':vcs_info:*' unstagedstr '%B%F{red}*%f%b '
zstyle ':vcs_info:*' stagedstr '%B%F{blue}+%f%b '
zstyle ':vcs_info:*' formats "%F{magenta}%b%f %u%c"
zstyle ':vcs_info:git:*' actionformats '(%b|%a%u%c)'
PROMPT='%B%(#.%F{red}%40<.../<%~%f%b.%F{blue}%40<.../<%~%f%b) ${vcs_info_msg_0_}'
RPROMPT='%F{magenta}%D{%H%M}%f'


#        |_)
#   _` | | |  _` |  __|
#  (   | | | (   |\__ \
# \__,_|_|_|\__,_|____/

if [[ $EUID != 0 ]] ; then
    source /home/m/vsc/bin/aliases
fi

## enable color support of ls and also add handy aliases.
if [ -x /usr/bin/dircolors ]
then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias -g ls='ls --color=auto'
    alias -g dir='dir --color=auto'
    alias -g vdir='vdir --color=auto'

    alias -g grep='grep --ignore-case --color=auto'
    alias -g fgrep='grep --ignore-case --color=auto'
    alias -g egrep='egrep --ignore-case --color=auto'

    alias -g diff='grc diff --color=auto'

    export LESS="-R"
    export LESS_TERMCAP_md=$'\e[01;31m'
    export LESS_TERMCAP_me=$'\e[0m'
    export LESS_TERMCAP_us=$'\e[01;32m'
    export LESS_TERMCAP_ue=$'\e[0m'
    export LESS_TERMCAP_so=$'\e[45;93m'
    export LESS_TERMCAP_se=$'\e[0m'
fi


## list aliasas
alias l='grc ls -1Bhl --color=always --group-directories-first' # '1' 4 one entry/line, 'B' ignores backups (~), 'h' 4 human readable (kiB, MiB, ...).
alias ll='grc ls -1ABhl --color=always --group-directories-first' # 'A' 4 almost all.
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
alias mnta='sudo mount -a; mount' # echo 4 color, semicolon 4 1. command, if ok, than 2. com.
alias uma='sudo umount -a; mount'

## pacman aliases
alias pi='sudo pacman -S' # install 1 pkg.
alias pp='sudo pacman -Syyu' # do a full system upgrade.
alias px='sudo pacman -R' # remove package.
alias pc='sudo pacman -Scc && sudo pacman-optimize' # remove all cached pkg! and defragment.
alias reflect='sudo reflector -p https -f 10 -l 10 --sort rate --save /etc/pacman.d/mirrorlist' # save 10 fastest of the 10 recent mirrors using https.

## file aliases
alias df='df -h'
alias countf='find . -type f | wc -l' # number of all files in dir.
alias countd='find . -type d | wc -l' # number of all subdirs in dir.

## launch alias
alias x='startx'
#alias evince='dbus-launch evince'
alias e="emacsclient -ca \'\'" # > service moved to systemd
alias scan='scanimage --format=tiff --mode=Color' #>http://lists.alioth.debian.org/pipermail/sane-devel/2001-December/001177.html
alias halt='sudo systemctl poweroff'
alias sus='sudo systemctl suspend'
#alias sus='umount /mnt/vsc3; umount /mnt/vsc4; sleep 10; systemctl suspend'
alias hib='sudo systemctl hibernate'

## misc
alias s='sudo su -'
alias c='clear'
alias rdark='razercfg -l all:off'
alias rlight='razercfg -l GlowingLogo:off -l Scrollwheel:on'
alias rename='perl-rename '
#alias zephyr='/usr/bin/git --git-dir=$HOME/.zephyr --work-tree=$HOME'
alias fifi='figlet -w 200 -f "shadow" '
alias cp='rsync -aP' # show percentage.
#alias ap='adb pull /sdcard/DCIM/Camera/ /mnt/troika/photos/2021/' more complicated: moved to bin.
alias d0='xrandr --auto'
alias d1='xrandr --output eDP --off --output HDMI-A-0 --auto'
alias d2='xrandr --output eDP --auto --output HDMI-A-0 --auto --right-of eDP'
alias d3='xrandr --output eDP --off --output DisplayPort-0 --scale 0.7 --output DisplayPort-1 --scale 0.7 --output HDMI-A-0 --scale 0.7'
alias pm='pulsemixer'
alias wh='which '
alias r='zranger'
alias scrot='scrot ~/.screens/%H%M%S.png'
alias p='python3 '
alias v='nvim '
alias vi='nvim '
alias vim='nvim '
alias gnome-session='echo "haha nice try:D" '


#              |  _)
#   _ \  __ \  __| |  _ \  __ \   __|
#  (   | |   | |   | (   | |   |\__ \
# \___/  .__/ \__|_|\___/ _|  _|____/
#       _|

setopt extendedglob # inverted expansion like: ls *~*.txt.
#setopt correct # correct mistakes.
setopt auto_list # list choice on ambiguous command.
setopt listtypes # %1 killed. will show up exactly when it is killed.
setopt auto_cd # change dir by just typing its name wo cd.
setopt auto_pushd # automatically adds dirs to stack.
setopt prompt_subst # prompt more dynamic, allow function in prompt.
setopt no_beep # never ever beep ever (alt: unsetopt beep).
setopt rm_star_wait # Wait, and ask if the user is serious when doing rm *.
#setopt complete_aliases # don't expand aliases _before_ completion has finished.
setopt append_history # Don't overwrite history.
setopt inc_append_history # saves in chronological order, all sessions.
setopt share_history # even more, sessioins share the same file!
setopt hist_ignore_all_dups # when runing a command several times, only store one.
setopt hist_reduce_blanks # reduce whitespace in history.
setopt hist_ignore_space # do not remember commands starting with space.
#setopt SH_WORD_SPLIT # word-splitting on unquoted parameter expansions by default.
HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000

#   _|                  |  _)
#  |   |   | __ \   __| __| |  _ \  __ \   __|
#  __| |   | |   | (    |   | (   | |   |\__ \
# _|  \__,_|_|  _|\___|\__|_|\___/ _|  _|____/

## sort filesize by size & in color:
dus() {
    paste <(du --exclude "./.*" --all --apparent-size --human-readable --max-depth=1 2>/dev/null | sed 's/\s.*//') <(ls --color=always -1 -U) | sort --human-numeric-sort
}

## sort all filesize by size & in color:
dusd() {
    paste <(du --apparent-size --all --human-readable --max-depth=1 2>/dev/null | sed 's/\s.*//') <(ls --color=always -1 --almost-all -U) | sort --human-numeric-sort
}

## sort size by filename (no color cause escape sequences mess that up):
duh() {
    du --apparent-size --exclude "./.*" --all --human-readable --max-depth=1 2>/dev/null | sort -t$'\t' -k2 --dictionary-order
}

## sort all size by filename (no color cause escape sequences mess that up):
duhd() {
    du --apparent-size --all --human-readable --max-depth=1 2>/dev/null | sort -t$'\t' -k2 --dictionary-order
}

rainbow() {
    for code in {0..255}
    do echo -e "\e[38;5;${code}m"'\\e[38;5;'"$code"m"\e[0m"
    done
}

## highlight help messages:
# help() {
#     "$@" --help 2>&1 | bathelp
# }

## Pressing enter in a git directory runs `git status`,
## in other directories `ls`.
magic-enter() {
    ## If commands are not already set, use the defaults.
    [ -z "$MAGIC_ENTER_GIT_COMMAND" ] && MAGIC_ENTER_GIT_COMMAND="git status ."
    [ -z "$MAGIC_ENTER_OTHER_COMMAND" ] && MAGIC_ENTER_OTHER_COMMAND="grc ls -1Bhl --color=always --group-directories-first ."
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


# run ls after typing cd:
function chpwd() {
    emulate -L zsh
    grc ls -1Bhl --color=always --group-directories-first . # runs ls (...) after typing cd!
}


## quick dir change:
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

# zranger() {
#     if [ -z "$RANGER_LEVEL" ]
#     then
#         . ranger # start in current dir
#     else
#         exit
#     fi
# }

# ## ranger-cd will fire for Ctrl+D :
# bindkey -s '^D' 'zranger\n'

# ## only read my config, do not add the defaults:
# export RANGER_LOAD_DEFAULT_RC=false

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


# |  _|
# | |
# | __|
#_|_|
#
# wrapper function to call "lf", a terminal filebrowser written in go.
# lf, which changes working dir in shell to last dir:
lf () {
    : Wrapper function to call "lf", a terminal filebrowser written in go.
    : Keep current working directory on exit.
    tmp="$(mktemp)"
    # pre-built binary, make sure to use absolute path:
    #/opt/sw/lf/bin/lf -config /opt/sw/lf/bin/lfrc -last-dir-path="$tmp" "$@"
    #command lf -last-dir-path="$tmp" "$@"

    if [ -f "$tmp" ]; then
        dir="$(cat "$tmp")"
        rm -f "$tmp"
        if [ -d "$dir" ]; then
            if [ "$dir" != "$(pwd)" ]; then
                cd "$dir"
            fi
        fi
    fi
}

## ueberzug:
alias lf="~/.config/lf/lfub"

# ## lf() will fire for 'Ctrl+D':
autoload -U lf # embedded in zshrc
bindkey -s '^D' "\eq lf\n"


#   _ \ __ \\ \   /
#   __/ |   |\ \ /
# \___|_|  _| \_/

export EDITOR='emacs'
export PATH='/home/m/bin:/home/m/vscloud/bin:/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin:/usr/local/sbin:/usr/games:/usr/local/games:/opt'
#PATH+=/scripts # hängt zur $path eben was an...
export GOPATH="$HOME/.go"

#       |          _|  _|
#   __| __| |   | |   |
# \__ \ |   |   | __| __|
# ____/\__|\__,_|_|  _|

## turn off XOFF/XON:
stty -ixon

## turn off powersaver/screensaver/blanking/bell:
xset -dpms s off s noblank -b &> /dev/null

## key setups:
bindkey -e # emacs key bindings
bindkey ' ' magic-space # also do history expansion on space, type '!!', then hit enter, watch.

## word jumping:
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

## auto suggestion:
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=6,bg=grey"
ZSH_AUTOSUGGEST_STRATEGY=(match_prev_cmd completion) # will first try to find a suggestion from your history, but, if it can't find a match, will find a suggestion from the completion engine (experimental).
ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20
ZSH_AUTOSUGGEST_USE_ASYNC=1

## history-substring-search:
source /usr/share/zsh/plugins/zsh-history-substring-search/zsh-history-substring-search.zsh

## autojump:
#source /etc/profile.d/autojump.zsh

## more colors! > manually copied:
#source ~/.zsh/zsh-dircolors/zsh-dircolors.plugin.zsh

## syntax highlighning has to be last:
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern cursor)

## autocompletion for lf:
fpath=(/home/m/.config/lf $fpath)

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

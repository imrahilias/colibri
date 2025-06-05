#!/bin/false
#blabla

#           |
# _  /  __| __ \   __| __|
#   / \__ \ | | | |   (
# ___|____/_| |_|_|  \___|
#

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

# sourcing "/home/m/.config/lf" stuff appearently raises "insecure
# directories" for compinit otherwise:
ZSH_DISABLE_COMPFIX="true"

# switch to custom keyboard layout:
# this could be set in .xinitrc but might not be loaded due to latency!
# enables multiple layouts, switch via awesome, us(m) is custom (see notes).
# also just drop the caps lock key, it is a second ctrl now.
setxkbmap -layout "us(m)" -option ctrl:nocaps

# set some theme options as global envs via sourcing this in .zashrc:
source "${XDG_CONFIG_HOME:-$HOME}/theme"


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
#

if [[ $EUID != 0 ]] ; then
    source /home/m/vsc/bin/aliases
fi

# enable color support of ls and also add handy aliases.
if [ -x /usr/bin/dircolors ]
then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias -g ls='ls --color=auto'
    alias -g dir='dir --color=auto'
    alias -g vdir='vdir --color=auto'

    alias -g grep='grep --ignore-case --color=auto'
    alias -g fgrep='grep --ignore-case --color=auto'
    alias -g egrep='egrep --ignore-case --color=auto'

    #alias -g diff='grc diff --color=auto'

    export LESS="-R"
    export LESS_TERMCAP_md=$'\e[01;31m'
    export LESS_TERMCAP_me=$'\e[0m'
    export LESS_TERMCAP_us=$'\e[01;32m'
    export LESS_TERMCAP_ue=$'\e[0m'
    export LESS_TERMCAP_so=$'\e[45;93m'
    export LESS_TERMCAP_se=$'\e[0m'
fi

# list aliasas
alias l='grc ls -1Bhl --color=always --group-directories-first' # '1' 4 one entry/line, 'B' ignores backups (~), 'h' 4 human readable (kiB, MiB, ...).
alias ll='grc ls -1ABhl --color=always --group-directories-first' # 'A' 4 almost all.
alias d='dirs -v' # lists zsh directory stack (enter <cd +- tab>, plus & minus (reverse) literally, with completion!.
alias blk='sudo blkid -o list'
alias hist='fc -El 0 | grep'
alias lsa='lsarchives '

# mount aliases
alias mnta='sudo mount -a; mount' # echo 4 color, semicolon 4 1. command, if ok, than 2. com.
alias uma='sudo umount -a; mount'

# pacman aliases
alias pi='sudo pacman -S' # install 1 pkg.
alias pp='sudo pacman -Syyu' # do a full system upgrade.
alias px='sudo pacman -R' # remove package.
alias pc='sudo pacman -Scc && sudo pacman-optimize' # remove all cached pkg! and defragment.
alias reflect='sudo reflector -p https -f 10 -l 10 --sort rate --save /etc/pacman.d/mirrorlist' # save 10 fastest of the 10 recent mirrors using https.

# file aliases
alias df='df -h'
alias countf='find . -type f | wc -l' # number of all files in dir.
alias countd='find . -type d | wc -l' # number of all subdirs in dir.

# launch alias
#alias evince='dbus-launch evince'
alias e="emacsclient -ca \'\'" # > service moved to systemd
alias scan='scanimage --format=tiff --mode=Color' #>http://lists.alioth.debian.org/pipermail/sane-devel/2001-December/001177.html
alias halt='sudo systemctl poweroff'
alias sus='sudo systemctl suspend'
#alias sus='umount /mnt/vsc3; umount /mnt/vsc4; sleep 10; systemctl suspend'
alias hib='sudo systemctl hibernate'

# misc
alias jour='journalctl -b | ccze'
alias s='sudo su -'
alias x='exit'
alias c='clear'
alias rdark='razercfg -l all:off'
alias rlight='razercfg -l GlowingLogo:off -l Scrollwheel:on'
alias rename='perl-rename '
#alias zephyr='/usr/bin/git --git-dir=$HOME/.zephyr --work-tree=$HOME'
alias fifi='figlet -w 200 -f "shadow" '
alias cp='rsync -aP' # show percentage.
#alias ap='adb pull /sdcard/DCIM/Camera/ /mnt/troika/photos/2021/' more complicated: moved to bin.
alias dauto='xrandr --auto --output DisplayPort-0 --scale 1 --output DisplayPort-1 --scale 1 --output HDMI-A-0 --scale 1'
alias dhome='xrandr --output eDP --off --output HDMI-A-0 --auto --primary --scale 1'
alias dleft='xrandr --output HDMI-A-0 --auto --primary --scale 1 --output eDP --auto --left-of HDMI-A-0'
alias dmid='xrandr --output HDMI-A-0 --auto --primary --scale 1 --output eDP --auto --below HDMI-A-0'
alias dwide='xrandr --output eDP --off --output DisplayPort-0 --auto --scale 0.7 --output DisplayPort-1 --auto --scale 0.7 --output HDMI-A-0 --auto --scale 0.7'
alias dtwo='xrandr --output eDP --off --output DisplayPort-0 --mode 3840x2160 --scale 0.7 --right-of HDMI-A-0'
alias wh='which '
alias r='zranger'
alias scrot='scrot ~/.screens/%H%M%S.png'
alias p='python3 '
alias v='nvim '
alias vi='nvim '
alias vim='nvim '
alias mnt=' mount | column -t'
#alias day='export THEME=light && source ~/bin/theme'
#alias night='export THEME=dark && source ~/bin/theme'
alias day='darkman set light'
alias night='darkman set dark'


# some gnome stuff:
alias gnome-session='echo "haha nice try:D"'
alias gnome-settings='LD_PRELOAD="" gnome-control-center' # gtk3-nocsd breaks gnome-control-center and possibly more...


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
#

# sort filesize by size & in color:
dus() {
    paste <(du --exclude "./.*" --all --apparent-size --human-readable --max-depth=1 2>/dev/null | sed 's/\s.*//') <(ls --color=always -1 -U) | sort --human-numeric-sort
}

# sort all filesize by size & in color:
dusd() {
    paste <(du --apparent-size --all --human-readable --max-depth=1 2>/dev/null | sed 's/\s.*//') <(ls --color=always -1 --almost-all -U) | sort --human-numeric-sort
}

# sort size by filename (no color cause escape sequences mess that up):
duh() {
    du --apparent-size --exclude "./.*" --all --human-readable --max-depth=1 2>/dev/null | sort -t$'\t' -k2 --dictionary-order
}

# sort all size by filename (no color cause escape sequences mess that up):
duhd() {
    du --apparent-size --all --human-readable --max-depth=1 2>/dev/null | sort -t$'\t' -k2 --dictionary-order
}

rainbow() {
    for code in {0..255}
    do echo -e "\e[38;5;${code}m"'\\e[38;5;'"$code"m"\e[0m"
    done
}

# highlight help messages:
# help() {
#     "$@" --help 2>&1 | bathelp
# }

# Pressing enter in a git directory runs `git status`,
# in other directories `ls`.
magic-enter() {
    # If commands are not already set, use the defaults.
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


# quick dir change:
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


# |  _|
# | |
# | __|
#_|_|
#

# ueberzug:
alias lf="~/.config/lf/lfub"

# lf() will fire for 'Ctrl+D':
autoload -U lf # embedded in zshrc
bindkey -s '^D' "\eq lf\n"


#                 _)
#  |   |  _` |_  / |
#  |   | (   |  /  |
# \__, |\__,_|___|_|
# ____/


function y() {
    local tmp="$(mktemp -t "yazi-cwd.XXXXXX")" cwd
    yazi "$@" --cwd-file="$tmp"
    if cwd="$(command cat -- "$tmp")" && [ -n "$cwd" ] && [ "$cwd" != "$PWD" ]; then
        builtin cd -- "$cwd"
    fi
    rm -f -- "$tmp"
}

#this doesnt work on zsh!
#change yazi's cwd to pwd on subshell exit:
# if [[ -n "$YAZI_ID" ]]; then
#     function _yazi_cd() {
#         ya pub dds-cd --str "$PWD"
#     }
#     add-zsh-hook zshexit _yazi_cd
# fi

# 'Ctrl+D' fires up yazi:
#bindkey -s "^y" "yy\n"
bindkey -s '^D' "\eq y\n"


#   _|     _|
#  | _  / |
#  __| /  __|
# _| ___|_|

#https://github.com/junegunn/fzf
#`--ansi` tells fzf to extract and parse ANSI color codes in the
# input, and it makes the initial scanning slower. So it's not
# recommended that you add it to your $FZF_DEFAULT_OPTS. `--nth` makes
# fzf slower because it has to tokenize each line.

# Set up fzf key bindings and fuzzy completion
source <(fzf --zsh)

# Enable execution tracing. For more details, refer to 'man zshbuiltins'
#typeset -ft fzf-completion

# Verbose Execution trace prompt (default: '+%N:%i> '). For more details, refer to 'man zshparam/zshmisc'
#PS4=$'\n%B%F{0}+ %D{%T:%3.} %2N:%I%f%b '


#
#   _ \ __ \\ \   /
#   __/ |   |\ \ /
# \___|_|  _| \_/
#

export EDITOR='emacs'
export PATH='/home/m/bin:/home/m/vscloud/bin:/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin:/usr/local/sbin:/usr/games:/usr/local/games:/opt'
#PATH+=/scripts # hängt zur $path eben was an...
export GOPATH="$HOME/.go"

# signal now fails:
#export SIGNAL_PASSWORD_STORE='gnome-libsecret'


#  |   |
#  __| __ \   _ \ __ `__ \   _ \
#  |   | | |  __/ |   |   |  __/
# \__|_| |_|\___|_|  _|  _|\___|
#

# this is changed via sed by darkman:
THEME_DARK=1

# set fzf options as global envs via sourcing this in .zshrc.
FZF_DEFAULT_OPTS_BASE="--style=minimal --no-info --no-separator --border=none --marker='█' --pointer='◆'"

# dark color palette:
FZF_DEFAULT_OPTS_DARK="--color=light --color=fg:#000000,fg+:#000000,bg:#FFFFFF,bg+:#FFFFFF,preview-bg:#FFFFFF,hl:#008080,hl+:#800080,info:#000000,marker:#800080,prompt:#800080,spinner:#330099,pointer:#800080,header:#000000,border:#000000,label:#000000,query:#000000,gutter:#FFFFFF"

# light color palette:
FZF_DEFAULT_OPTS_LIGHT="--color=dark --color=fg:#FAF0E6,fg+:#FAF0E6,bg:#000000,bg+:#1D1F21,preview-bg:#000000,hl:#00FFFF,hl+:#FF00FF,info:#FAF0E6,marker:#FF00FF,prompt:#FF00FF,spinner:#330099,pointer:#FF00FF,header:#FAF0E6,border:#FAF0E6,label:#FAF0E6,query:#FAF0E6,gutter:#000000"

# dark side or light side?
if [[ $THEME_DARK == 1 ]]
then
    export CALIBRE_USE_DARK_PALETTE=1

    export FZF_DEFAULT_OPTS="$FZF_DEFAULT_OPTS_BASE $FZF_DEFAULT_OPTS_DARK"

    # term colors:
    xrdb -merge ~/.config/darkman/dark
    xrdb -merge ~/.Xresources

    # qt looks like current gtk theme
    # qt themes use the gtk2/3 theme arc-blackest converted via qt6gtk2,
    # has to be run from yay whenever either qt6 or gtk2/3 change,
    # no way to do that for two themes in the background though.
    export QT_QPA_PLATFORMTHEME="gtk2" # qt looks like current gtk theme
    export QT_STYLE_OVERRIDE="gtk2"

else
    export CALIBRE_USE_DARK_PALETTE=0

    export FZF_DEFAULT_OPTS="$FZF_DEFAULT_OPTS_BASE $FZF_DEFAULT_OPTS_LIGHT"

    # term colors:
    xrdb -merge ~/.config/darkman/light
    xrdb -merge ~/.Xresources

    # qt themes use the gtk2/3 theme arc-blackest converted via qt6gtk2:
    export QT_QPA_PLATFORMTHEME="gtk2"
    export QT_STYLE_OVERRIDE="gtk2"
fi


#       |          _|  _|
#   __| __| |   | |   |
# \__ \ |   |   | __| __|
# ____/\__|\__,_|_|  _|
#

# turn off XOFF/XON:
stty -ixon

# dumb lock:
hasssid=`nmcli d show wlan0 | grep "GENERAL.CONNECTION:" | awk '{print $2}'`
if [[ $hasssid = "internetz" ]] | [[ $hasssid = "tephelon" ]] ; then
    # turn off powersaver/screensaver/blanking/bell:
    xset -dpms
    xset s off
    xset s noblank
else
    # start dimmer after 300s and lock after 10 more s:
    xset s 300 10
    xset +dpms
fi
xset -b &> /dev/null # turn off bell

# key setups:
bindkey -e # emacs key bindings
bindkey ' ' magic-space # also do history expansion on space, type '!!', then hit enter, watch.

# word jumping:
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

# auto suggestion:
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=6,bg=grey"
ZSH_AUTOSUGGEST_STRATEGY=(match_prev_cmd completion) # will first try to find a suggestion from your history, but, if it can't find a match, will find a suggestion from the completion engine (experimental).
ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20
ZSH_AUTOSUGGEST_USE_ASYNC=1

# history-substring-search:
source /usr/share/zsh/plugins/zsh-history-substring-search/zsh-history-substring-search.zsh

# autojump:
#source /etc/profile.d/autojump.zsh

# more colors! > manually copied:
#source ~/.zsh/zsh-dircolors/zsh-dircolors.plugin.zsh

# syntax highlighning has to be last:
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern cursor)

# autocompletion for lf:
#fpath=(/home/m/.config/lf $fpath)

# 1password unlock:
export OP_BIOMETRIC_UNLOCK_ENABLED=true

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
zstyle ':completion:*:kill:*' force-list always
zstyle ':completion:*:processes' command "ps -au${USER}"
zstyle ':completion:*:sudo:*' command-path /bin /usr/bin /usr/local/bin /home/m/bin /sbin /usr/sbin /usr/local/sbin /usr/games /usr/local/games
zstyle ':completion:*:parameters' list-colors '=*=1;36'
zstyle ':completion:*:options' list-colors '=^(-- *)=34'
zstyle ':completion:*:commands' list-colors '=*=1;33'
zstyle ':completion:*:builtins' list-colors '=*=1;38;5;142'
zstyle ':completion:*:aliases' list-colors '=*=2;38;5;128'
zstyle ':completion:*:*:kill:*' list-colors '=(#b) #([0-9]#)*( *[a-z])*=34=31=33'
#zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'

eval "$(op completion zsh)"; compdef _op op # 1pasword cli completion

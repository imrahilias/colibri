#!/bin/false
#blabla

# profiling: Add this to the TOP of your .zshrc:
#zmodload zsh/zprof

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

if [[ $EUID != 0 ]] ; then
    path=(~/bin ~/asc/bin $path) # Zsh ties the PATH variable to a path array.
    source /home/m/asc/bin/aliases
    source "$HOME/asc/bin/api.conf" # llm chat.ai.tuwien.ac.at api for emacs
    source "$HOME/.xprofile"
fi


#        |_)
#   _` | | |  _` |  __|
#  (   | | | (   |\__ \
# \__,_|_|_|\__,_|____/
#

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

    export LESS="-R"
    export LESS_TERMCAP_md=$'\e[01;31m'
    export LESS_TERMCAP_me=$'\e[0m'
    export LESS_TERMCAP_us=$'\e[01;32m'
    export LESS_TERMCAP_ue=$'\e[0m'
    export LESS_TERMCAP_so=$'\e[45;93m'
    export LESS_TERMCAP_se=$'\e[0m'
fi

# list aliasas
alias l='grc ls -1Bhl --color=always --group-directories-first' # one entry/line, ignores backups (~), human readable (kiB, MiB, ...).
alias ll='grc ls -1ABhl --color=always --group-directories-first' # almost all.
alias d='dirs -v' # lists zsh directory stack (enter <cd +- tab>, plus & minus (reverse) literally, with completion!
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
# sort filesize by size & in color:
alias dus="paste <(du --exclude './.*' --all --apparent-size --human-readable --max-depth=1 2>/dev/null | sed 's/\s.*//') <(ls --color=always -1 -U) | sort --human-numeric-sort"
# sort all filesize by size & in color:
alias dusd="paste <(du --apparent-size --all --human-readable --max-depth=1 2>/dev/null | sed 's/\s.*//') <(ls --color=always -1 --almost-all -U) | sort --human-numeric-sort"
# sort size by filename (no color cause escape sequences mess that up):
alias duh="du --apparent-size --exclude './.*' --all --human-readable --max-depth=1 2>/dev/null | sort -t$'\t' -k2 --dictionary-order"
# sort all size by filename (no color cause escape sequences mess that up):
alias duhd="du --apparent-size --all --human-readable --max-depth=1 2>/dev/null | sort -t$'\t' -k2 --dictionary-order"

# launch alias
#alias evince='dbus-launch evince'
alias e="emacsclient -ca \'\'" # > service moved to systemd
alias scan='scanimage --format=tiff --mode=Color' #>http://lists.alioth.debian.org/pipermail/sane-devel/2001-December/001177.html
alias halt='sudo systemctl poweroff'
alias sus='sudo systemctl suspend'
alias hib='sudo systemctl hibernate'

# misc
alias jour='journalctl -b | ccze'
alias s='sudo su -'
alias x='exit'
alias c='clear'
alias rdark='razercfg -l all:off'
alias rlight='razercfg -l GlowingLogo:off -l Scrollwheel:on'
alias rename='perl-rename '
alias fifi='figlet -w 200 -f "shadow" '
alias cp='rsync -aP' # show percentage.
alias dauto='xrandr --auto --output DisplayPort-0 --scale 1 --output DisplayPort-1 --scale 1 --output HDMI-A-0 --scale 1'
alias dhome='xrandr --output eDP --off --output HDMI-A-0 --auto --primary --scale 1'
alias dleft='xrandr --output HDMI-A-0 --auto --primary --scale 1 --output eDP --auto --left-of HDMI-A-0'
alias dmid='xrandr --output HDMI-A-0 --auto --primary --scale 1 --output eDP --auto --below HDMI-A-0'
alias dwide='xrandr --output eDP --off --output DisplayPort-0 --auto --scale 0.7 --output DisplayPort-1 --auto --scale 0.7 --output HDMI-A-0 --auto --scale 0.7'
alias dlap='xrandr --output eDP --auto  --output --primary --scale 1 --output DisplayPort-0 off --output DisplayPort-1 off --output HDMI-A-0 off --output HDMI-A-1 off'
alias dtwo='xrandr --output eDP --off --output DisplayPort-0 --mode 3840x2160 --scale 0.7 --right-of HDMI-A-0'
alias wh='which '
alias r='zranger'
alias scrot='scrot ~/.screens/%H%M%S.png'
alias p='python3 '
alias v='nvim '
alias vi='nvim '
alias vim='nvim '
alias mnt=' mount | column -t'
alias down='yt-dlp --downloader aria2c --downloader-args "-c -j 3 -x 3 -s 3 -k 1M" '

# some gnome stuff:
alias gnome-session='echo "haha nice try:D"'
alias gnome-settings='LD_PRELOAD="" gnome-control-center' # gtk3-nocsd breaks gnome-control-center and possibly more...

#              |  _)
#   _ \  __ \  __| |  _ \  __ \   __|
#  (   | |   | |   | (   | |   |\__ \
# \___/  .__/ \__|_|\___/ _|  _|____/
#       _|

stty -ixon # turn off XOFF/XON
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

autoload -Uz add-zsh-hook

# term title
function xterm_title_precmd () {
    print -Pn -- '\e]2;%n@%m %~\a'
    [[ "$TERM" == 'screen'* ]] && print -Pn -- '\e_\005{2}%n\005{-}@\005{5}%m\005{-} \005{+b 4}%~\005{-}\e\\'
}

function xterm_title_preexec () {
    print -Pn -- '\e]2;%n@%m %~ %# ' && print -n -- "${(q)1}\a"
    [[ "$TERM" == 'screen'* ]] && { print -Pn -- '\e_\005{2}%n\005{-}@\005{5}%m\005{-} \005{+b 4}%~\005{-} %# ' && print -n -- "${(q)1}\e\\"; }
}

if [[ "$TERM" == (Eterm*|alacritty*|aterm*|foot*|gnome*|konsole*|kterm*|putty*|rxvt*|screen*|wezterm*|tmux*|xterm*) ]]; then
    add-zsh-hook -Uz precmd xterm_title_precmd
    add-zsh-hook -Uz preexec xterm_title_preexec
fi

# faster on demand rehash (https://wiki.archlinux.org/title/Zsh):
zshcache_time="$(date +%s%N)"
rehash_precmd() {
  if [[ -a /var/cache/zsh/pacman ]]; then
    local paccache_time="$(date -r /var/cache/zsh/pacman +%s%N)"
    if (( zshcache_time < paccache_time )); then
      rehash
      zshcache_time="$paccache_time"
    fi
  fi
}
add-zsh-hook -Uz precmd rehash_precmd

# highlight help messages:
help() {
    "$@" --help 2>&1 | bat --color=always --language=help
}

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

# run ls after typing cd:
function chpwd() {
    emulate -L zsh
    grc ls -1Bhl --color=always --group-directories-first .
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

# ueberzug:
#alias lf="~/.config/lf/lfub"

# lf() will fire for 'Ctrl+D':
#autoload -U lf # embedded in zshrc
#bindkey -s '^D' "\eq lf\n"

function y() {
    local tmp="$(mktemp -t "yazi-cwd.XXXXXX")" cwd
    yazi "$@" --cwd-file="$tmp"
    if cwd="$(command cat -- "$tmp")" && [ -n "$cwd" ] && [ "$cwd" != "$PWD" ]; then
        builtin cd -- "$cwd"
    fi
    rm -f -- "$tmp"
}

function day() {
    # term colors:
    xrdb ~/.Xresources
    xrdb -merge ~/.config/darkman/light
    darkman set light
}

function night() {
    # term colors:
    xrdb ~/.Xresources
    xrdb -merge ~/.config/darkman/dark
    darkman set dark
}

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

# Debug: Enable execution tracing. For more details, refer to 'man zshbuiltins'
#typeset -ft fzf-completion

# Debug: Verbose Execution trace prompt (default: '+%N:%i> '). For more details, refer to 'man zshparam/zshmisc'
#PS4=$'\n%B%F{0}+ %D{%T:%3.} %2N:%I%f%b '

#       |          _|  _|
#   __| __| |   | |   |
# \__ \ |   |   | __| __|
# ____/\__|\__,_|_|  _|
#

# dumb lock:
hasssid=`nmcli d show wlp3s0 | grep "GENERAL.CONNECTION:" | awk '{print $2}'`
if [[ $hasssid = "internetz" || $hasssid = "tephelon" ]] ; then
    # turn off powersaver/screensaver/blanking/bell.
    xset -dpms
    xset s blank
    xset s off
else
    # start dimmer after 300s and lock after 10 more s.
    xset +dpms
    xset s 300 10
    xset s on
fi
xset -b &> /dev/null # turn off bell

#        |            _)
#  __ \  | |   |  _` | | __ \   __|
#  |   | | |   | (   | | |   |\__ \
#  .__/ _|\__,_|\__, |_|_|  _|____/
# _|            |___/

# uncomment only in case cleared cache thows plugin not found errors.
antidote_load () {
    source '/usr/share/zsh-antidote/antidote.zsh'
    antidote load
}

# Set the root name of the plugins files (.txt and .zsh) antidote will use.
zsh_plugins=${ZDOTDIR:-~}/.zsh_plugins

# Ensure the .zsh_plugins.txt file exists so you can add plugins.
[[ -f ${zsh_plugins}.txt ]] || touch ${zsh_plugins}.txt

# Lazy-load antidote from its functions directory.
fpath=(/usr/share/zsh-antidote/functions $fpath)
autoload -Uz antidote

# Generate a new static file whenever .zsh_plugins.txt is updated.
if [[ ! ${zsh_plugins}.zsh -nt ${zsh_plugins}.txt ]]; then
  antidote bundle <${zsh_plugins}.txt >|${zsh_plugins}.zsh
fi

# Source the static plugins file.
source ${zsh_plugins}.zsh

ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=white, bg=default"
ZSH_AUTOSUGGEST_STRATEGY=(match_prev_cmd completion)
ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20
ZSH_AUTOSUGGEST_USE_ASYNC=1

# collides with completion?
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern cursor)

# |
# |  /  _ \ |   |  __|
#   <   __/ |   |\__ \
#_|\_\\___|\__, |____/
#          ____/

bindkey -e # emacs key bindings
bindkey ' ' magic-space # also do history expansion on space like '!!'
bindkey "^M" magic-enter
bindkey . rationalize-dot
bindkey -s '^D' "\eq y\n"
bindkey -s '^\' "\eq rga-fzf\n"

# always fzf for everything is maybe not what i want.
#bindkey '^I' fzf_completion

# You are using zsh in MULTIBYTE mode to support modern character sets (for
# languages other than English).  To use the Meta or Alt keys, you probably
# need to revert to single-byte mode with a command such as:
unsetopt MULTIBYTE

# create a zkbd compatible hash; to add other keys to this hash, see:
# man 5 terminfo:
typeset -g -A key

# find the suffix nuber at https://man.archlinux.org/man/user_caps.5
key[Home]="${terminfo[khome]}"
key[End]="${terminfo[kend]}"
key[Insert]="${terminfo[kich1]}"
key[Backspace]="${terminfo[kbs]}"
key[Delete]="${terminfo[kdch1]}"
key[Up]="${terminfo[kcuu1]}"
key[Down]="${terminfo[kcud1]}"
key[Left]="${terminfo[kcub1]}"
key[Right]="${terminfo[kcuf1]}"
key[PageUp]="${terminfo[kpp]}"
key[PageDown]="${terminfo[knp]}"
key[Shift-Tab]="${terminfo[kcbt]}"
key[Control-Left]="${terminfo[kLFT5]}"
key[Control-Right]="${terminfo[kRIT5]}"
key[Alt-Left]="${terminfo[kLFT3]}"
key[Alt-Right]="${terminfo[kRIT3]}"

# setup key accordingly:
[[ -n "${key[Home]}"      ]] && bindkey -- "${key[Home]}"       beginning-of-line
[[ -n "${key[End]}"       ]] && bindkey -- "${key[End]}"        end-of-line
[[ -n "${key[Insert]}"    ]] && bindkey -- "${key[Insert]}"     overwrite-mode
[[ -n "${key[Backspace]}" ]] && bindkey -- "${key[Backspace]}"  backward-delete-char
[[ -n "${key[Delete]}"    ]] && bindkey -- "${key[Delete]}"     delete-char
[[ -n "${key[Up]}"        ]] && bindkey -- "${key[Up]}"         up-line-or-history
[[ -n "${key[Down]}"      ]] && bindkey -- "${key[Down]}"       down-line-or-history
[[ -n "${key[Left]}"      ]] && bindkey -- "${key[Left]}"       backward-char
[[ -n "${key[Right]}"     ]] && bindkey -- "${key[Right]}"      forward-char
[[ -n "${key[PageUp]}"    ]] && bindkey -- "${key[PageUp]}"     beginning-of-buffer-or-history
[[ -n "${key[PageDown]}"  ]] && bindkey -- "${key[PageDown]}"   end-of-buffer-or-history
[[ -n "${key[Shift-Tab]}" ]] && bindkey -- "${key[Shift-Tab]}"  reverse-menu-complete
[[ -n "${key[Control-Left]}"  ]] && bindkey -- "${key[Control-Left]}"  backward-word
[[ -n "${key[Control-Right]}" ]] && bindkey -- "${key[Control-Right]}" forward-word
[[ -n "${key[Alt-Left]}"  ]] && bindkey -- "${key[Alt-Left]}"  backward-word
[[ -n "${key[Alt-Right]}" ]] && bindkey -- "${key[Alt-Right]}" forward-word


# Finally, make sure the terminal is in application mode, when zle is
# active. Only then are the values from $terminfo valid.
if (( ${+terminfo[smkx]} && ${+terminfo[rmkx]} )); then
    autoload -Uz add-zle-hook-widget
    function zle_application_mode_start { echoti smkx }
    function zle_application_mode_stop { echoti rmkx }
    add-zle-hook-widget -Uz zle-line-init zle_application_mode_start
    add-zle-hook-widget -Uz zle-line-finish zle_application_mode_stop
fi

# or run zkbd: `autoload zkbd; zkbd` to find out which key maps to which caracter.
#source ~/.zkbd/$TERM-${${DISPLAY:t}:-$VENDOR-$OSTYPE}

# arrow keys & page up/dpwn in history (fzf too):
autoload -Uz up-line-or-beginning-search down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search

[[ -n "${key[Up]}"   ]] && bindkey -- "${key[Up]}"   up-line-or-beginning-search
[[ -n "${key[Down]}" ]] && bindkey -- "${key[Down]}" down-line-or-beginning-search

#
#   __|  _ \  __ `__ \  __ \
#  (    (   | |   |   | |   |
# \___|\___/ _|  _|  _| .__/
#                      _|

# 1pasword cli completion depends on compinit:
#eval "$(op completion zsh)"; compdef _op op


zstyle ':completion:*' cache-path ~/.config/shell/zsh_cache
zstyle ':completion:*' complete-options true # add dirstack to `cd -` completion
zstyle ':completion:*' completer _match _expand _complete _correct _approximate
zstyle ':completion:*' completions 1
zstyle ':completion:*' file-sort name
zstyle ':completion:*' glob 1
zstyle ':completion:*' group-name '' # group in topic blocks, otherwise the colors have no effect
zstyle ':completion:*' insert-unambiguous true
zstyle ':completion:*' max-errors 2
zstyle ':completion:*' menu select search
zstyle ':completion:*' original true
zstyle ':completion:*' rehash false # faster on demand
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' sort true
zstyle ':completion:*' special-dirs true # tab-completion for .. and others
zstyle ':completion:*' squeeze-slashes true
zstyle ':completion:*' squeeze-slashes true # expand //tmp to /tmp instead of /*/tmp
zstyle ':completion:*' substitute 1
zstyle ':completion:*' use-cache on
zstyle ':completion:*' verbose true
zstyle ':completion:*:*:*:*:aliases' format '%F{blue}%B%d%b%f'
zstyle ':completion:*:*:*:*:builtins' format '%F{yellow}%B%d%b%f'
#zstyle ':completion:*:*:*:*:commands' format '%F{yellow}%B%d%b%f' # that is almost everything, all binaries, etc
zstyle ':completion:*:*:*:*:corrections' format '%F{yellow}%B%d (errors: %e)%b %f'
zstyle ':completion:*:*:*:*:functions' format '%F{magenta}%B%d%b%f'
zstyle ':completion:*:*:*:*:options' format '%F{blue}%B%d%b%f'
zstyle ':completion:*:*:*:*:parameters' format '%F{green}%B%d%b%f'
zstyle ':completion:*:*:-command-:*:*' group-order alias builtins functions commands
zstyle ':completion:*:*:kill:*' list-colors '=(#b) #([0-9]#)*( *[a-z])*=34=31=33'
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:*:kill:*:processes' command 'ps haxopid:5,user:4,%cpu:4,ni:2,stat:3,etime:8,args'
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:aliases' list-colors '=*=34'
zstyle ':completion:*:builtins' list-colors '=*=33'
#zstyle ':completion:*:commands' list-colors '=*=33' # that is almost everything, all binaries, etc
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*:descriptions' format '%B%d%b'
zstyle ':completion:*:functions' list-colors '=*=35'
zstyle ':completion:*:kill:*' force-list always
zstyle ':completion:*:match:*' original only
zstyle ':completion:*:options' list-colors '=^(-- *)=34' # This will present all command options in blue, but description of that options will stay normal.
zstyle ':completion:*:parameters' list-colors '=*=1;32'
zstyle ':completion:*:processes' command "ps -au${USER}"
zstyle ':completion:*:sudo:*' command-path /bin /usr/bin /usr/local/bin /home/m/bin /sbin /usr/sbin /usr/local/sbin /usr/games /usr/local/games
zstyle ':completion::complete:*' gain-privileges 1 # This will let Zsh completion scripts run commands with sudo privileges. You should not enable this if you use untrusted autocompletion scripts.

#  |   |                          |
#  __| __ \   _ \   _ \ __ \   _` |
#  |   | | |  __/   __/ |   | (   |
# \__|_| |_|\___| \___|_|  _|\__,_|
#

# Add the following to the end of ~/.zshrc:
eval "$(starship init zsh)"

# Add this to the BOTTOM of your .zshrc
#zprof

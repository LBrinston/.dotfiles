#!/bin/bash

# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
    *) return;;
esac

# XDG Base Directory Specification Variables
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_STATE_HOME="$HOME/.local/state"
export XDG_CACHE_HOME="$HOME/.cache"

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTFILE="${XDG_STATE_HOME}"/bash/history
HISTSIZE=1000
HISTFILESIZE=2000
HISTOCONTROL=ignoredups

# Check for fzf
REQUIRED_PKG="fzf"
PKG_OK=$(dpkg-query -W --showformat='${Status}\n' $REQUIRED_PKG|grep "install ok installed")
#echo Checking for $REQUIRED_PKG: $PKG_OK
if [ "" = "$PKG_OK" ]; then
    echo "No $REQUIRED_PKG. Setting up $REQUIRED_PKG."
    sudo apt-get --yes install $REQUIRED_PKG
else
    # Place source for fzf config files here
    source ~/.fzf/key-bindings.bash
    source ~/.fzf/completion.bash
    #[ -f ~/.fzf.bash ] && source ~/.fzf.bash
fi

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	    # We have color support; assume it's compliant with Ecma-48
	    # (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	    # a case would tend to support setf rather than setaf.)
	    color_prompt=yes
    else
	    color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]$(__git_ps1 " (%s)")\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# Turn off the terminal bell
bind 'set bell-style visible'

# If this is an xterm set the title to user@host:dir
case "$TERM" in
    xterm*|rxvt*)
        PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
        ;;
    *)
        ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'

fi

# colored GCC warnings and errors
#export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
    if [ -f /usr/share/bash-completion/bash_completion ]; then
        . /usr/share/bash-completion/bash_completion
    elif [ -f /etc/bash_completion ]; then
        . /etc/bash_completion
    fi
fi

# Fancy opening
# https://stackoverflow.com/questions/1298066/how-can-i-check-if-a-package-is-installed-and-install-it-if-not
# Check that we're the first shell spawned - otherwise skip the fancy-ness
LIVE_COUNTER=$(ps a | awk '{print $2}' | grep -vi "tty*" | uniq | wc -l);
if [ $LIVE_COUNTER -eq 1 ]; then

    REQUIRED_PKG="fastfetch"
    PKG_OK=$(dpkg-query -W --showformat='${Status}\n' $REQUIRED_PKG|grep "install ok installed")
    #echo Checking for $REQUIRED_PKG: $PKG_OK
    if [ "" = "$PKG_OK" ]; then
        echo "No $REQUIRED_PKG. Setting up $REQUIRED_PKG."
        sudo apt-add sudo add-apt-repository ppa:zhangsongcui3371/fastfetch
        sudo apt install $REQUIRED_PKG
    else
        LIVE_COUNTER=$(ps a | awk '{print $2}' | grep -vi "tty*" | uniq | wc -l);
        if [ $LIVE_COUNTER -eq 1 ]; then
            fastfetch
        fi
    fi
fi

# If starship is install then let's use it
if which starship > /dev/null; then
    echo "Starship detected! 🚀"
    eval "$(starship init bash)"
else
    echo "No starship detected."
fi;

# Checking CPU architecture
# cut -f 2 d ":" remove the part of the line before :
# awk '{$1=$1}1' removes the space from the beginning of the line
CPU_INFO=$(lscpu | grep 'Model name' | cut -f 2 -d ":" | awk '{$1=$1}1')
if [ "Intel(R) Core(TM) i5-7500 CPU @ 3.40GHz" = "$CPU_INFO" ]; then
    # We're on a desktop - test should maybe be more specific?
    #elif [];then
    true # Nop until we decide how we want to use this

fi

# Bash completion
# https://github.com/scop/bash-completion/
source /etc/profile.d/bash_completion.sh

# Vterm Syncrhonization with emacs
vterm_printf() {
    if [ -n "$TMUX" ] \
        && { [ "${TERM%%-*}" = "tmux" ] \
            || [ "${TERM%%-*}" = "screen" ]; }; then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

# Default Editor
export EDITOR="emacs -nw"
export VISUAL="doom-emacs"

# Check if bat is installed - note: the bin is named batcat on debian sys because naming conflict
if dpkg -s bat > /dev/null 2>&1; then
    # Set batcat as the paginator for man
    # May need to set MANROFFOPT="-c" if you experience formatting issues
    export MANPAGER="sh -c 'col -bx | batcat -l man -p'"
fi

export LC_ALL=en_CA.UTF-8
export LANG=en_CA.UTF-8
export LANGUAGE=En_CA:en_GB:en

# Environment variables
export SEMESTER="spring_2025"
export UVIC="$HOME/Documents/UVic"

export ANDROID_USER_HOME="$XDG_DATA_HOME/android:$PATH"
# Rust config

## Cargo conf
export CARGO_HOME="$XDG_DATA_HOME/cargo"
export PATH="$XDG_DATA_HOME/cargo/bin:$PATH"
export DOCKER_CONFIG="$XDG_CONFIG_HOME"/docker
export DOCKER_CONFIG="$XDG_CONFIG_HOME"/dotnet
export GNUPGHOME="$XDG_DATA_HOME"/gnupg
# Adding to PATH
export PATH="$HOME/.local/bin:$PATH"

# Scripts
export PATH="$HOME/.bin/:$PATH"
export PATH="$HOME/scripts/:$PATH"
export PATH="/$HOME/scripts/file_manip:$PATH"
export PATH="/$HOME/scripts/hardware:$PATH"
export PATH="/$HOME/scripts/WIP:$PATH"
export PATH="/$HOME/scripts/rclone:$PATH"
export PATH="/$HOME/scripts/rclone/filters:$PATH"
export PATH="$HOME/scripts/notifications:$PATH"
export PATH="$HOME/scripts/sdr:$PATH"

# Microcontrollers
## Pi Pico
export PATH="/opt/arm-none-eabi/bin:$PATH"
# Python XDG vars
## For python >v3.13.0a3
export PYTHONSTARTUP="$XDG_CONFIG_HOME"/python/pythonrc
## For python <v3.13.0a3
export PYTHON_HISTORY="$XDG_CONFIG_HOME"/python/python_history

# Path to Python virtual environments
export PATH="$HOME/venvs:$PATH"
## Doom Emacs
export PATH="$XDG_CONFIG_HOME/doom:$PATH"
export PATH="$XDG_CONFIG_HOME/doom-emacs/bin/:$PATH"
export OCTAVE_HISTFILE="$XDG_STATE_HOME/octave_hist"
export CUDA_CACHE_PATH="$XDG_CACHE_HOME"/nv
alias wget=wget --hsts-file="$XDG_DATA_HOME/wget-hsts"
export WINEPREFIX="$XDG_DATA_HOME"/wine

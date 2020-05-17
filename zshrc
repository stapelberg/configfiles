# vim:ts=4:sw=4:expandtab
# Load 4000 lines of history, but save O(∞)
HISTSIZE=4000
HISTFILE=~/.zsh_history
SAVEHIST=10000000
# Print timing statistics for everything which takes longer than 5 seconds of
# user + system time ('sleep 6' does not work because of 0% user/system time!).
REPORTTIME=5
# Do not save duplicate entries
setopt HIST_IGNORE_DUPS
setopt INC_APPEND_HISTORY
setopt COMPLETE_IN_WORD
# For the git branch stuff later on.
setopt EXTENDED_GLOB
# Do not check mail, I don’t use mail that way.
unsetopt mail_warning
unset MAILCHECK
# Do not share history (enabled by default in NixOS’s /etc/zshrc).
unsetopt SHARE_HISTORY

setopt short_loops

# All unquoted arguments of the form `anything=expression' appearing after the
# command name have filename expansion (that is, where expression has a leading
# `~' or `=')  performed on expression as if it were a parameter assignment.
setopt MAGIC_EQUAL_SUBST

export EDITOR=~/configfiles/zmacsclient
export VISUAL=$EDITOR
export PAGER='less -R'
export GTK_IM_MODULE=xim
export VTYSH_PAGER='cat'
# long date format in ls(1)
export TIME_STYLE=long-iso
# prefer library/system calls/programming manuals
export MANSECT="8:3:2:3posix:3pm:3perl:1:n:l:5:4:9:6:7"

export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[00;32m'

# Explicitly select Emacs line editing mode (independent of zsh’s best guess
# based on EDITOR/VISUAL).
bindkey -e

# Make M-<backspace> delete to (e.g.) the previous slash, just like Emacs.
autoload -U select-word-style
select-word-style bash
# For compatibility with my previous config:
bindkey "^T" backward-kill-word

bindkey "^X^F" push-line

# Press C-x C-e to edit the command line in emacs.
autoload -z edit-command-line
zle -N edit-command-line
bindkey "^X^E" edit-command-line

# NO BEEPING!
setopt no_BEEP

# Don't display an error if there are no matches, I know what I am doing
setopt no_NOMATCH

# Leave processes open when closing a shell with background processes
setopt no_HUP

# Skip .o-files when completing for vi
fignore=(.o)

# On debian, App::Ack is installed as ack-grep, so alias it.
# Where ag (silversearcher) is available, prefer that (much faster).
local _ack_location="$(\unalias -a; command -v ag ack ack-grep 2>/dev/null | head -1)"
[ $? = 0 ] && alias ack="$_ack_location"

export LS_COLORS='di=01;94:ln=01;96:pi=33:so=01;95:bd=01;93:cd=01;93:ex=01;92:do=01;95:su=37;41:sg=30;43:st=37;44:ow=34;42:tw=30;42:ca=30;41'
alias spr="curl -F 'sprunge=<-' http://sprunge.us"

# Requires liburi-perl and xclip.
up() {
    for file in $*; do
        # Ensure the file is world-readable before uploading
        chmod o+r $file
        scp $file ex62:htdocs/
        # Echo and try to put this into the X11 clipboard, too
        perl -MURI::Escape -E 'print "https://t.zekjur.net/" . uri_escape(shift)' \
            "$(basename "$file")" | tee >(xclip) && echo
    done
}

# Clones the git sources of a Debian package
# needs debcheckout from devscripts and gbp-clone from git-buildpackage
d-clone() {
    local package=$1
    if ! debcheckout --auth --print $package >/dev/null
    then
        echo "debcheckout $package failed. Is $package missing Vcs tags?"
	return
    fi

    set -- $(debcheckout --auth --print $package)
    if [ "$1" != "git" ]
    then
        echo "$package does not use git, but $1 instead."
        return
    fi

    echo "cloning $2"
    gbp clone --pristine-tar $2 || return

    # Change to the newest git repository
    cd $(dirname $(ls -1td */.git | head -1)) || return

    echo "d-clone set up everything successfully."
}

# Nicer output of ls
alias ls='ls --color=auto'
alias ll='ls -hl'
alias l='ll'
alias rt='ls -hltr'
alias L='dpkg -L'
alias v='vim'
alias V='sudo vim'
e() { ~/configfiles/zmacsclient "$@" }
E() { e /sudo::${1} }
alias m='make'
alias mp='mplayer -really-quiet'
# disable gdb welcome message
alias gdb='gdb -q'
alias sctl='s systemctl'
alias j='journalctl --full -e -u'
alias d='~/go/bin/distri'
alias D='sudo distri'
# Find files in current folder
f() {
    q="*$1*"
    find . -iname $q
}
alias h='cat ~/.zsh_history{_*,} | grep --text --color '

# write $1 to the inserted sdcard
sdcard() {
    sudo dd of=/dev/disk/by-id/usb-Generic-_USB3.0_CRW_-SD_201006010301-0:0 bs=64k oflag=dsync status=progress if="$1"
}

# Edit a temporary file with my template for a C proof-of-concept
alias cpoc='cd /tmp && FN=$(mktemp --suffix=.c) && cp ~/configfiles/poc-template.c $FN && vi $FN && echo "Proof-of-concept stored in $FN"'

alias bc='bc -ql'
alias diff='diff --color'

alias cal='cal -y'

# More passwords, faster!
alias pw='pwgen -s 23 1'

alias pd="perldoc"

alias ripcd='cdparanoia -Bs 1-'

x() {  xclip -i <<< $($*) }

alias asdf='/home/michael/toggle_layout.sh'
alias uiae='/home/michael/toggle_layout.sh'

# A nicer ps-output. We need to specify user:12 because otherwise usernames
# such as 'sphinxsearch' or 'libvirt-qemu' are displayed as user IDs. According
# to the procps source, this is specified in The Open Group Base Specifications
# Issue 6 (IEEE Std 1003.1, 2004 Edition).
alias p='ps -A f -o user:12,pid,priority,ni,pcpu,pmem,args'

alias nh='unset HISTFILE'

alias acs='apt-cache search'
alias acsn='apt-cache search --names-only'
alias acp='apt-cache policy'
alias acsh='apt-cache show'
alias ac='apt-cache'
alias ys='yum search'
agi() { sudo apt install $* && rehash }
yi() { sudo yum install $* && rehash }
_da() { _deb_packages uninstalled; }
alias agu='sudo apt update'
alias agdu='sudo apt full-upgrade'

agr() { sudo apt remove $* && rehash }

alias smi='sudo make install'

git() { =git $* && git_branch_chdir }
alias g='git'
alias s='sudo'
# make filename absolute, i.e. prepend current working directory
abs() { echo -n $PWD/$1 }
alias 6='s ip -6'
alias 4='s ip -4'
alias -g T="| tail"
alias -g G="| grep"
alias -g H="| head"
# copy to clipboard
alias -g C="| xclip"

# run command in an i3 tabbed split container
t() { i3-msg -q split vertical, layout tabbed && $@; i3-msg -q move up }

ping() {
    if [ $# -eq 0 ]
    then
        =ping google.ch
    else
        =ping "$@"
    fi
}

# Replaces prompt (hostname, pwd) by "% " in each line in the clipboard
alias strip-xclip-prompt="xclip -out | sed 's,^$HOST [^\$]* \\\$ ,% ,g' | xclip"

# Use ~sl in any command, for example less ~sl
hash -d pb=/var/cache/pbuilder/result
hash -d dcs=~/go/src/github.com/Debian/dcs
hash -d pk4=~/go/src/github.com/Debian/pk4
hash -d ratt=~/go/src/github.com/Debian/ratt
hash -d rirc=~/go/src/github.com/robustirc/robustirc
hash -d router7=~/go/src/github.com/rtr7/router7
hash -d gokrazy=~/go/src/github.com/gokrazy/gokrazy
hash -d scan2drive=~/go/src/github.com/stapelberg/scan2drive
hash -d debiman=~/go/src/github.com/Debian/debiman

set_termtitle() {
    # escape '%' chars in $1, make nonprintables visible
    local a=${(V)1//\%/\%\%}

    # Truncate command, and join lines.
    a=${a//[$'\r'$'\n']/}

    [ "$a" = "zsh" ] && { a=${(%)${:-%~}} }

    case $TERM in
    screen*)
        # plain xterm title
        print -rn -- $'\e'"]2;${(%)${:-%m}}: $a"$'\a'

        # screen title (in ^A")
        print -rn -- $'\e'"k$a"$'\e'$'\\'

        # screen location
        print -rn -- $'\e'"_${(%)${:-%m}}: $a"$'\e'$'\\'
    ;;
    xterm*|rxvt*)
        # plain xterm title
        print -rn -- $'\e'"]2;${(%)${:-%m}}: $a"$'\a'
    ;;
    esac
}

my_prompt_precmd() {
    set_termtitle "zsh"
}

my_prompt_preexec() {
    set_termtitle "$1"
}

typeset -ga precmd_functions
precmd_functions+=my_prompt_precmd

typeset -ga preexec_functions
preexec_functions+=my_prompt_preexec

cwd_to_urxvt() {
    local update="\0033]777;cwd-spawn;path;$PWD\0007"

    case $TERM in
    screen*)
    # pass through to parent terminal emulator
        update="\0033P$update\0033\\";;
    esac

    echo -ne "$update"
}

cwd_to_urxvt # execute upon startup to set initial directory
chpwd_functions=(${chpwd_functions[@]} cwd_to_urxvt)

# Define prompt
fg_green=$'%{\e[1;32m%}'
fg_light_blue=$'%{\e[1;36m%}'
fg_light_silver=$'%{\e[1;37m%}'
fg_white=$'%{\e[1;37m%}'
fg_no_colour=$'%{\e[0m%}'
lvl=$SHLVL
case $TERM in
screen)
lvl=$((lvl-1))
;;
esac

cfgfiles=${${:-~/.zshrc}:A:h}

export __CURRENT_GIT_PROMPT=

setup_prompt() {
    local _main_fmt
    local _cfg_nag

    if [ $USER = "michael" -o $USER = "stapelberg" ]; then
        _main_fmt="%m"
    else
        _main_fmt="%n@%m"
    fi

    if [ -f "${cfgfiles}/ERROR" ]; then
        _cfg_nag="%F{red}cfg-git-error%f "
    else
        _cfg_nag=""
    fi

    if [ $lvl -ge 2 ] ; then
        PROMPT="%K{cyan}%F{black}$_main_fmt%k%f ${fg_green}%~${fg_no_colour} ${__CURRENT_GIT_PROMPT}$lvl $_cfg_nag%% "
    else
        PROMPT="%K{cyan}%F{black}$_main_fmt%k%f ${fg_green}%~${fg_no_colour} ${__CURRENT_GIT_PROMPT}$_cfg_nag%% "
    fi

    # For tramp (emacs).
    if [ "$TERM" = "dumb" ]; then
        unset PROMPT
        PS1='$ '
        unsetopt zle
    fi
}

# show the git branch in prompt
typeset -a __CURRENT_GIT_DIR
parse_git_branch() {
    local fg_dark_blue=$'%{\e[0;36m%}'
    local fg_no_colour=$'%{\e[0m%}'

    if [ -f ${__CURRENT_GIT_DIR[1]}/HEAD ]
    then
        local branch=$(sed 's/ref: refs\/heads\///g' ${__CURRENT_GIT_DIR[1]}/HEAD)
        __CURRENT_GIT_PROMPT="${fg_dark_blue}${branch}${fg_no_colour} "
    else
        __CURRENT_GIT_PROMPT=""
    fi
}

git_branch_chdir() {
    __CURRENT_GIT_DIR=((../)#.git)
    parse_git_branch
    setup_prompt
}

git_branch_chdir
chpwd_functions=(${chpwd_functions[@]} git_branch_chdir)

setup_prompt

# Export language
unset LC_ALL
export LANG=de_DE.UTF-8
export LC_MESSAGES=C
# Skip costly locale -a | grep on machines which are known to have reasonable
# locales configured.
if [ "$HOST" = "midna" ]
then
    export LC_TIME=en_DK.UTF-8
else
    if locale -a | grep -q en_DK.UTF-8; then
        export LC_TIME=en_DK.UTF-8
    fi
fi

# TODO: remove once Go ≥ 1.8 is widespread enough (which defaults to ~/go)
export GOPATH=~/go
# Expand path to /usr/sbin and /sbin (because i know which binaries i can call)
# Add $GOPATH/bin
export PATH=/home/michael/.config/avail:/home/michael/.cargo/bin:$GOPATH/bin:~/.local/bin:~/.bin:$PATH:/usr/sbin:/sbin

# For debian utilities
export DEBEMAIL="stapelberg@debian.org"
export DEB_BUILD_OPTIONS="parallel=8"
export QUILT_PATCHES=debian/patches

# System-wide default settings for cmake, so that the default
# “cd build && cmake .. && ninja” invocation works:
# (Requires cmake ≥v3.17.0)
export CMAKE_EXPORT_COMPILE_COMMANDS=ON
export CMAKE_GENERATOR=Ninja

# When tab-completing, show dots. For fast tab completes, they will be
# overwritten instantly, for long tab-completions, you have feedback.
expand-or-complete-with-dots() {
    echo -n -e "\e[37m...\e[0m\033[3D"
    zle expand-or-complete
    zle redisplay
}

load-completion() {
    autoload compinit
    compinit -C
    compdef _da agi

    # 'ctrl-x r' will complete the 12 last modified (mtime) files/directories
    zle -C newest-files menu-complete _generic
    # Use “*newest-files” so that it matches both “newest-files” and
    # “load-completion-and-newest-files”.
    zstyle ':completion:*newest-files:*' completer _files
    zstyle ':completion:*newest-files:*' file-patterns '*(omN[1,12])'
    zstyle ':completion:*newest-files:*' menu select yes
    zstyle ':completion:*newest-files:*' sort false
    zstyle ':completion:*newest-files:*' matcher-list 'b:=*' # important

    # Show all processes when completing kill/killall and enable menu mode
    zstyle ':completion:*:processes' command 'ps f -N --ppid=$(pidof kthreadd) --pid=$(pidof kthreadd)'
    zstyle ':completion:*:processes-names' command 'ps -aeo comm='
    zstyle ':completion:*:*:kill:*' menu yes select
    zstyle ':completion:*:*:killall:*' menu yes select

    # colors for zsh file name completion
    zmodload zsh/complist
    zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}

    bindkey '\t' expand-or-complete-with-dots
    bindkey '^Xr' newest-files
}

load-completion-and-expand-or-complete-with-dots() {
    load-completion
    zle -w expand-or-complete-with-dots
}

load-completion-and-newest-files() {
    load-completion
    zle -w newest-files
}

zle -N expand-or-complete-with-dots
zle -N load-completion-and-expand-or-complete-with-dots
zle -N load-completion-and-newest-files
bindkey '\t' load-completion-and-expand-or-complete-with-dots
bindkey '^Xr' load-completion-and-newest-files

# Enable url-quote-magic to automatically escape URLs when pasting
autoload -U url-quote-magic
zle -N self-insert url-quote-magic

# Have a bell-character put out, everytime a command finishes. This will set the urgent-hint,
# if the terminal is configured accordingly
bellchar=$'\a'
zle-line-init () { echo -n "$bellchar" }
zle -N zle-line-init

# Disable C-s stopping output — triggered way more often accidentally.
stty -ixon

# Directory specific configuration
chpwd_profiles() {
    if [[ "$PWD" =~ "$HOME/i3($|/|/*)" ]]
    then
        alias m='CC=clang make -j16'
    else
        alias m='make'
    fi

    if [[ "$PWD" =~ "$HOME/d/(pkg|out)/([^/]*)" ]]
    then
        export pkg=$HOME/d/pkg/$match[2]
        export out=$HOME/d/out/$match[2]
    else
        unset pkg
        unset out
    fi
}
chpwd_functions=( ${chpwd_functions[@]} chpwd_profiles )
# Call this function before the first chpwd. This is necessary to get correct
# aliases in subshells (VIM’s :sh for example).
chpwd_profiles

[ -e "$HOME/.zshrc_host" ] && source ~/.zshrc_host

# If the configfiles are in a git repository, update if it’s older than one hour.
# On x1/x200, I am running cfgupdater instead which triggers on a network connection.
[ "$HOST" != "x1" -a "$HOST" != "x200" ] && ( sleep 1; exec $cfgfiles/gocode/bin/configfiles -configfiles_dir=$cfgfiles -quiet ) &!

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

setopt short_loops

# All unquoted arguments of the form `anything=expression' appearing after the
# command name have filename expansion (that is, where expression has a leading
# `~' or `=')  performed on expression as if it were a parameter assignment.
setopt MAGIC_EQUAL_SUBST

export EDITOR='vim'
export VISUAL='vim'
export PAGER='less'
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

# Explicitly select vim line editing mode. This is necessary for SSH sessions
# where EDITOR/VISUAL are not forwarded and thus zsh assumes emacs line editing
# mode by default.
bindkey -v

# C-r is easier to type than Esc-k /
bindkey "^R" history-incremental-search-backward
bindkey "^[[A" up-line-or-history
bindkey "^[[B" down-line-or-history
bindkey "^F" push-line

# When tab-completing, show dots. For fast tab completes, they will be
# overwritten instantly, for long tab-completions, you have feedback.
expand-or-complete-with-dots() {
    echo -n -e "\e[37m...\e[0m\033[3D"
    zle expand-or-complete
    zle redisplay
}
zle -N expand-or-complete-with-dots
bindkey "^I" expand-or-complete-with-dots

# C-t deletes to the left of the cursor until the next /. Useful to delete a
# path component.
backward-delete-to-slash() {
  integer pos=$CURSOR
  while (( pos > 1 )); do
    if [[ ${LBUFFER[pos]} = / ]]; then
      LBUFFER=${LBUFFER[0,pos]}
      return 0
    fi
    pos=$((pos-1))
  done
  return 1
}
zle -N backward-delete-to-slash
bindkey "^T" backward-delete-to-slash

# Press 'v' to edit the command line in vim.
autoload -U edit-command-line
zle -N edit-command-line
bindkey -M vicmd v edit-command-line

# NO BEEPING!
setopt no_BEEP

# Don't display an error if there are no matches, I know what I am doing
setopt no_NOMATCH

# Leave processes open when closing a shell with background processes
setopt no_HUP

# Skip .o-files when completing for vi
fignore=(.o)

# On debian, App::Ack is installed as ack-grep, so alias it
which ack-grep >/dev/null 2>&1 && alias ack='ack-grep'
# Also, docker is docker.io
which docker.io >/dev/null 2>&1 && alias docker='docker.io'

# colors for ls and (more importantly) zsh completion
zmodload zsh/complist
export LS_COLORS='di=01;34:ln=01;36:pi=33:so=01;35:bd=01;33:cd=01;33:ex=01;32:do=01;35:su=37;41:sg=30;43:st=37;44:ow=34;42:tw=30;42:ca=30;41'
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}

# 'ctrl-x r' will complete the 12 last modified (mtime) files/directories
zle -C newest-files complete-word _generic
bindkey '^Xr' newest-files
zstyle ':completion:newest-files:*' completer _files
zstyle ':completion:newest-files:*' file-patterns '*~.*(omN[1,12])'
zstyle ':completion:newest-files:*' menu select yes
zstyle ':completion:newest-files:*' sort false
zstyle ':completion:newest-files:*' matcher-list 'b:=*' # important

alias spr="curl -F 'sprunge=<-' http://sprunge.us"

# Requires liburi-perl and xclip.
function up() {
    for file in $*; do
        # Ensure the file is world-readable before uploading
        chmod o+r $file
        scp $file alp:/media/persistent/t.zekjur.net/
        # Echo and try to put this into the X11 clipboard, too
        perl -MURI::Escape -E 'print "http://t.zekjur.net/" . uri_escape(shift)' \
            "$(basename "$file")" | tee >(xclip) && echo
    done
}

# Prepares a Debian upload in /tmp/up/. Pass a .changes file.
function prep-deb-up() {
    abspath=$(readlink -f $1)
    [ -d /tmp/up ] && rm -rf /tmp/up
    mkdir /tmp/up
    cd /tmp/up
    dget file://$abspath
    debsign -k4AC8EE1D *changes
}

# Clones the git sources of a Debian package
# needs debcheckout from devscripts and gbp-clone from git-buildpackage
function d-clone() {
    local package=$1
    local giturl
    if debcheckout --print $package >/dev/null
    then
        set -- $(debcheckout --print $package)
        if [ "$1" != "git" ]
        then
            echo "$package does not use git, but $1 instead."
            return
        fi

        # The check out URL is different from the push URL.
        # These transformations try to get to the push URL.
        giturl=$(echo $2 | sed 's/^git/git+ssh/' | sed 's/anonscm\.debian\.org/git.debian.org/' | sed 's,debian\.org,debian.org/git,')

        echo "cloning $giturl"
        gbp clone --pristine-tar $giturl || return

        # Change to the newest git repository
        cd $(dirname $(ls -1td */.git | head -1)) || return

        # This tells git to push all branches at once,
        # i.e. if you changed upstream and debian (after git-import-orig),
        # both upstream and debian will be pushed when running “git push”.
        git config push.default matching || return

        git config user.email "stapelberg@debian.org" || return

        # This tells git to push tags automatically,
        # so you don’t have to use “git push && git push --tags”.
        git config --add remote.origin.push "+refs/heads/*:refs/heads/*" || return
        git config --add remote.origin.push "+refs/tags/*:refs/tags/*" || return

        echo "d-clone set up everything successfully."
    else
        echo "debcheckout $package failed. Is $package missing Vcs tags?"
    fi
}

# i3am <patch-id> switches to the i3 directory and merges the patch
# Recompilation should be done manually in order to negative-test any new
# testcases first.
function i3am() {
    cd ~/i3 && (curl http://cr.i3wm.org/patch/$1/raw | git am -3 --whitespace=fix)
}

# Nicer output of ls
alias ls='ls --color=auto'
alias ll='ls -hl'
alias l='ll'
alias rt='ls -hltr'
alias L='dpkg -L'
alias v='vim'
alias V='sudo vim'
alias m='make'
alias mp='mplayer -really-quiet'
# disable gdb welcome message
alias gdb='gdb -q'
alias sctl='s systemctl'
alias j='journalctl --full -e -u'
# Find files in current folder
function f() {
    q="*$1*"
    find . -iname $q
}

alias wl="sudo sh -c 'IF_WPA_VERBOSITY=1 wpa_action wlan0 DISCONNECTED ; ifdown --force wlan0 ; ifup wlan0'"

# Debug the last coredump
alias dbg='~/.bin/gdb-coredump.pl'

# Edit a temporary file with my template for a C proof-of-concept
alias cpoc='cd /tmp && FN=$(mktemp --suffix=.c) && cp ~/configfiles/poc-template.c $FN && vi $FN && echo "Proof-of-concept stored in $FN"'

alias bc='bc -ql'

alias ü='cd /home/michael/Uni/S4/Uebungen'

alias cal='cal -y'

# More passwords, faster!
alias pw='pwgen -s 23 1'

alias pd="perldoc"

# Open terminal in ISO-8859-15-Mode
alias iso='LANG=en_US.iso885915 LC_ALL=en_US.iso885915 urxvt'

alias ripcd='cdparanoia -Bs 1-'
alias cam="mplayer -v tv:// -tv device=/dev/video0:driver=v4l2:outfmt=yuy2"

function x() {  xclip -i <<< $($*) }

# Prepend a command with e to close the starting shell
function e() {
    eval "$* &|"
    exit
}

function wb() {
    mkdir -p ~/Bilder/whiteboard/$(date +'%Y-%m-%d')
    cd ~/Bilder/whiteboard/$(date +'%Y-%m-%d')
    gphoto2 -P
    chmod 644 *
}

# wiipdf with the ID of my primary wiimote
alias wp="wiipdf 00:19:1D:93:CA:EB "

alias asdf='/home/michael/toggle_layout.sh'
alias uiae='/home/michael/toggle_layout.sh'

alias update-mirror='debmirror --diff=none --getcontents --passive --verbose --progress --nosource --host=ftp.de.debian.org --dist=squeeze,sid --arch=amd64 debian && sudo apt-get update'
alias update-mirror-multimedia='debmirror debian-multimedia --diff=none --getcontents --passive --ignore-small-errors --verbose --progress --ignore-release-gpg --host=www.debian-multimedia.org --dist=stable,testing --arch=amd64 --root=/ --method=http --section=main'

# Burn a single file onto CD-ROM
alias burnfile='mkisofs ${1} | cdrecord driveropts=burnfree -v fs=6m -'
function burndvd {
    growisofs -dvd-compat -Z /dev/sr0=${1}
}

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
alias ag='s apt-get'
alias ys='yum search'
function agi { sudo apt-get install $* && rehash }
function yi { sudo yum install $* && rehash }
_da() { _deb_packages uninstalled; }
alias agu='sudo apt-get update'
alias agdu='sudo apt-get dist-upgrade'
alias cupt-upgrade='sudo cupt -o debug::resolver=1 -i -V -D -R full-upgrade i3-wm/installed sudo/installed 2>&1 | tee /tmp/cupt.log'

function agr { sudo apt-get remove $* && rehash }

alias smi='sudo make install'

alias g='git'
alias s='sudo'
alias 6='s ip -6'
alias 4='s ip -4'
alias -g T="| tail"
alias -g G="| grep"
alias -g H="| head"

# Use ~sl in any command, for example less ~sl
hash -d sl=/var/log/syslog
hash -d pb=/var/cache/pbuilder/result
hash -d dcs=~/gocode/src/github.com/Debian/dcs
hash -d rirc=~/gocode/src/github.com/robustirc/robustirc

# aliases for my IRC / mail screens which request a kerberos ticket if no valid
# one is present
alias irc='(klist -s || kinit) && ssh labs -t screen -Dr irc'
alias mail='(klist -s || kinit) && ssh midna -t screen -x sup'
alias mpd='(4 a G 172.22.37 >/dev/null && ncmpcpp -h 172.22.37.1) || (4 a G 172.22.36 >/dev/null && ncmpcpp -h 172.22.36.117) || echo "Not sure which MPD to use"'

# Lock the screen and suspend to RAM.
alias susp='i3lock -i ~/Bilder/triforce2560.png -t && sudo sh -c "echo mem > /sys/power/state"'

# show the git branch in prompt
export __CURRENT_GIT_BRANCH=
typeset -a __CURRENT_GIT_DIR
parse_git_branch() {
    [ -f ${__CURRENT_GIT_DIR[1]}/HEAD ] && sed 's/ref: refs\/heads\///g' ${__CURRENT_GIT_DIR[1]}/HEAD
}

git_branch_chdir() {
    __CURRENT_GIT_DIR=((../)#.git)
}

git_branch_chdir
chpwd_functions=(${chpwd_functions[@]} git_branch_chdir)

get_git_prompt_info() {
    fg_dark_blue=$'%{\e[0;36m%}'
    fg_no_colour=$'%{\e[0m%}'

    if [ ! -z "$__CURRENT_GIT_BRANCH" ]
    then
        echo "${fg_dark_blue}$__CURRENT_GIT_BRANCH${fg_no_colour} "
    else
        echo ""
    fi
}

function set_termtitle() {
    # escape '%' chars in $1, make nonprintables visible
    a=${(V)1//\%/\%\%}

    # Truncate command, and join lines.
    a=$(print -rn -- "$a" | tr -d "\n\r")

    [ "$a" = "zsh" ] && { a=$(print -Pn "%~") }

    case $TERM in
    screen)
        # plain xterm title
        print -Pn -- "\e]2;$2: "
        print -rn -- "$a"
        print -n -- "\a"

        # screen title (in ^A")
        print -n -- "\ek"
        print -rn -- "$a"
        print -n -- "\e\\"

        # screen location
        print -Pn -- "\e_$2: "
        print -rn -- "$a"
        print -n -- "\e\\"
    ;;
    xterm*|rxvt)
        # plain xterm title
        print -Pn -- "\e]2;$2: "
        print -rn -- "$a"
        print -n -- "\a"
    ;;
    esac
}

function my_prompt_precmd() {
    export __CURRENT_GIT_BRANCH="$(parse_git_branch)"
    set_termtitle "zsh" "%m"
}

function my_prompt_preexec() {
    set_termtitle "$1" "%m"
}

typeset -ga precmd_functions
precmd_functions+=my_prompt_precmd

typeset -ga preexec_functions
preexec_functions+=my_prompt_preexec

function chpwd() {
    export __CURRENT_GIT_BRANCH="$(parse_git_branch)"
}


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


# Convenience wrapper around /etc/init.d
function in() {
    sudo /etc/init.d/$*
}

function scan() {
       scanimage --format TIFF --resolution 300 -x 215 -y 297 > $1.tiff && convert $1.tiff -resize 1024x $1.jpg
}

# Start mplayer with some options for nicer streaming on the given channel
function tv {
# -ni and -noidx: disable index (streams are not seekable)
# -framedrop: it's ok to drop some frames, MPEG-TS is a lossy format
# -vf kerndeint: specify deinterlacing for those broadcasters which don't do it themselves
# -prefer-ipv4: we don't have ipv6, so don't try
# -xineramascreen 0: needed for correct aspect-ratio
# -cache 4096: stream stutters if we don't buffer a bit
    mplayer -noidx -framedrop -vf kerndeint -prefer-ipv4 -cache 2048 http://tv:7000/${1}
}

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

# enable substitution in prompt, necessary for $(get_git_prompt_info)
setopt prompt_subst

setup_prompt() {
    local _main_fmt
    local _cfg_nag

    if [ $USER = "michael" ]; then
        _main_fmt="%m"
    else
        _main_fmt="%n@%m"
    fi

    if [ -f "$(dirname $(readlink ~/.zshrc))/ERROR" ]; then
        _cfg_nag="%F{red}cfg-git-error%f "
    else
        _cfg_nag=""
    fi

    if [ $lvl -ge 2 ] ; then
        PROMPT="%K{cyan}%F{black}$_main_fmt%k%f ${fg_green}%~${fg_no_colour} \$(get_git_prompt_info)$lvl $_cfg_nag$ "
    else
        PROMPT="%K{cyan}%F{black}$_main_fmt%k%f ${fg_green}%~${fg_no_colour} \$(get_git_prompt_info)$_cfg_nag$ "
    fi
}

setup_prompt

# Export language
unset LC_ALL
export LANG=de_DE.UTF-8
export LC_MESSAGES=C
if locale -a | grep -q en_DK.UTF-8; then
    export LC_TIME=en_DK.UTF-8
fi

export GOPATH=~/gocode
# Expand path to /usr/sbin and /sbin (because i know which binaries i can call)
# Add $GOPATH/bin
export PATH=/home/michael/go/bin:$GOPATH/bin:~/.local/bin:~/.bin:$PATH:/usr/sbin:/sbin

# For debian utilities
export DEBEMAIL="stapelberg@debian.org"

# Initialize completion
autoload compinit
compinit -C
compdef _da agi

# Enable url-quote-magic to automatically escape URLs when pasting
autoload -U url-quote-magic
zle -N self-insert url-quote-magic

# Have a bell-character put out, everytime a command finishes. This will set the urgent-hint,
# if the terminal is configured accordingly
bellchar=$'\a'
zle-line-init () { echo -n "$bellchar" }
zle -N zle-line-init

# Show all processes when completing kill/killall and enable menu mode
zstyle ':completion:*:processes' command 'ps f -N --ppid=$(pidof kthreadd) --pid=$(pidof kthreadd)'
zstyle ':completion:*:processes-names' command 'ps -aeo comm='
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:*:killall:*' menu yes select

# Directory specific configuration
function chpwd_profiles() {
    if [[ ${PWD} =~ "$HOME/i3($|/|/*)" ]]
    then
        alias m='CC=clang make -j16'
    else
        alias m='make'
    fi

    if [[ ${PWD} =~ "$HOME/DPKG($|/|/*)" ]]
    then
        export GIT_AUTHOR_EMAIL="stapelberg@debian.org"
    else
        unset GIT_AUTHOR_EMAIL
    fi
}
chpwd_functions=( ${chpwd_functions[@]} chpwd_profiles )
# Call this function before the first chpwd. This is necessary to get correct
# aliases in subshells (VIM’s :sh for example).
chpwd_profiles

# Initialize SSH completion only with hosts in my ~/.ssh/config, but especially with the aliases
# I gave them (and the full host names).
[ -e "$HOME/.ssh/config" ] && zstyle ':completion:*:complete:ssh:*:hosts' hosts $(sed -n "s/^[ \\t]*Host\(name\|\) \(.*\)/\\2/p" $HOME/.ssh/config | uniq)

[ -e "$HOME/.zshrc_host" ] && source ~/.zshrc_host

cfgfiles=$(dirname $(readlink ~/.zshrc))
# If the configfiles are in a git repository, update if it’s older than one hour.
# On x1/x200, I am running cfgupdater instead which triggers on a network connection.
[ "$HOST" != "x1" -a "$HOST" != "x200" ] && $cfgfiles/gocode/bin/configfiles -quiet &!

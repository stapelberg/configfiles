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
  # When right next to a /, delete the / followed by the next path component.
  # There are two conditionals because for an as-of-yet undiscovered reason,
  # only the first one works on my work computer and only the second one works
  # on my private computer. Both use the same version and user-config of zsh.
  # Likely a system-level config difference.
  if [[ pos > 2 && ${LBUFFER[pos-1]} = / ]]; then
    pos=$((pos-2))
  fi
  if [[ pos > 1 && ${LBUFFER[pos]} = / ]]; then
    pos=$((pos-1))
  fi
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
[ -f /usr/bin/ack-grep ] && alias ack='ack-grep'

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
up() {
    for file in $*; do
        # Ensure the file is world-readable before uploading
        chmod o+r $file
        scp $file alp:/media/persistent/t.zekjur.net/
        # Echo and try to put this into the X11 clipboard, too
        perl -MURI::Escape -E 'print "http://t.zekjur.net/" . uri_escape(shift)' \
            "$(basename "$file")" | tee >(xclip) && echo
    done
}

# Clones the git sources of a Debian package
# needs debcheckout from devscripts and gbp-clone from git-buildpackage
d-clone() {
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
f() {
    q="*$1*"
    find . -iname $q
}
alias h='cat ~/.zsh_history | grep --text --color '

# Edit a temporary file with my template for a C proof-of-concept
alias cpoc='cd /tmp && FN=$(mktemp --suffix=.c) && cp ~/configfiles/poc-template.c $FN && vi $FN && echo "Proof-of-concept stored in $FN"'

alias bc='bc -ql'

alias cal='cal -y'

# More passwords, faster!
alias pw='pwgen -s 23 1'

alias pd="perldoc"

alias ripcd='cdparanoia -Bs 1-'

x() {  xclip -i <<< $($*) }

# Prepend a command with e to close the starting shell
e() {
    eval "$* &|"
    exit
}

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
alias ag='s apt-get'
alias ys='yum search'
agi() { sudo apt-get install $* && rehash }
yi() { sudo yum install $* && rehash }
_da() { _deb_packages uninstalled; }
alias agu='sudo apt-get update'
alias agdu='sudo apt-get dist-upgrade'

agr() { sudo apt-get remove $* && rehash }

alias smi='sudo make install'

alias g='git'
alias s='sudo'
alias 6='s ip -6'
alias 4='s ip -4'
alias -g T="| tail"
alias -g G="| grep"
alias -g H="| head"

# Use ~sl in any command, for example less ~sl
hash -d pb=/var/cache/pbuilder/result
hash -d dcs=~/gocode/src/github.com/Debian/dcs
hash -d rirc=~/gocode/src/github.com/robustirc/robustirc

set_termtitle() {
    # escape '%' chars in $1, make nonprintables visible
    local a=${(V)1//\%/\%\%}

    # Truncate command, and join lines.
    a=${a//[$'\r'$'\n']/}

    [ "$a" = "zsh" ] && { a=${(%)${:-%~}} }

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

my_prompt_precmd() {
    set_termtitle "zsh" "%m"
}

my_prompt_preexec() {
    set_termtitle "$1" "%m"
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

zshrc=~/.zshrc
cfgfiles=${zshrc:A:h}

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
        PROMPT="%K{cyan}%F{black}$_main_fmt%k%f ${fg_green}%~${fg_no_colour} ${__CURRENT_GIT_PROMPT}$lvl $_cfg_nag$ "
    else
        PROMPT="%K{cyan}%F{black}$_main_fmt%k%f ${fg_green}%~${fg_no_colour} ${__CURRENT_GIT_PROMPT}$_cfg_nag$ "
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
if locale -a | grep -q en_DK.UTF-8; then
    export LC_TIME=en_DK.UTF-8
fi

export GOPATH=~/gocode
# Expand path to /usr/sbin and /sbin (because i know which binaries i can call)
# Add $GOPATH/bin
export PATH=/home/michael/go/bin:$GOPATH/bin:~/.local/bin:~/.bin:$PATH:/usr/sbin:/sbin

# For debian utilities
export DEBEMAIL="stapelberg@debian.org"

load-completion() {
    if [ "x${zshLoadedCompletion}" = "x" ]; then
        zshLoadedCompletion='done'
        autoload compinit
        compinit -C
        compdef _da agi
        bindkey '\t' complete-word
        zle complete-word
    fi
}
zle -N load-completion
bindkey '\t' load-completion

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
chpwd_profiles() {
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

[ -e "$HOME/.zshrc_host" ] && source ~/.zshrc_host

# If the configfiles are in a git repository, update if it’s older than one hour.
# On x1/x200, I am running cfgupdater instead which triggers on a network connection.
[ "$HOST" != "x1" -a "$HOST" != "x200" ] && $cfgfiles/gocode/bin/configfiles -configfiles_dir=$cfgfiles -quiet &!

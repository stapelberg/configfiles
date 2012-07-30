# Save 2000 lines of history
HISTSIZE=4000
HISTFILE=~/.zsh_history
SAVEHIST=4000
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

# C-r is easier to type than Esc-k /
bindkey "^R" history-incremental-search-backward

bindkey -M viins >&-
bindkey -M viins "^[[A" up-line-or-history
bindkey -M viins "^[[B" down-line-or-history
bindkey -M viins "^F" push-line

# When tab-completing, show dots. For fast tab completes, they will be
# overwritten instantly, for long tab-completions, you have feedback.
expand-or-complete-with-dots() {
	echo -n -e "\e[37m...\e[0m\033[3D"
	zle expand-or-complete
	zle redisplay
}
zle -N expand-or-complete-with-dots
bindkey "^I" expand-or-complete-with-dots

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
which ack-grep >/dev/null && alias ack='ack-grep'

# colors for ls and (more importantly) zsh completion
zmodload zsh/complist
export LS_COLORS='di=01;34:ln=01;36:pi=33:so=01;35:bd=01;33:cd=01;33:ex=01;32:do=01;35:su=37;41:sg=30;43:st=37;44:ow=34;42:tw=30;42:ca=30;41'
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}

# Nicer output of ls
alias ls='ls --color=auto'
alias ll='ls -hl'
alias l='ll'
alias rt='ls -hltr'
alias L='dpkg -L'
alias v='vim'
alias V='sudo vim'
alias m='make'
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

# A nicer ps-output
alias p='ps -A f -o user,pid,priority,ni,pcpu,pmem,args'

alias nh='unset HISTFILE'

alias acs='apt-cache search'
alias acsn='apt-cache search --names-only'
alias acp='apt-cache policy'
alias acsh='apt-cache show'
alias ac='apt-cache'
alias ag='s apt-get'
function agi { sudo apt-get install $* && rehash }
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

alias yt='clive --stream-exec="mplayer %i;" --stream 20 '

# Use ~sl in any command, for example less ~sl
hash -d sl=/var/log/syslog
hash -d pb=/var/cache/pbuilder/result

# aliases for my IRC / mail screens which request a kerberos ticket if no valid
# one is present
alias irc='(klist -s || kinit) && ssh labs -t screen -Dr irc'
alias mail='(klist -s || kinit) && ssh midna -t screen -x sup'
alias mpd='(4 a G 172.22.37 >/dev/null && ncmpcpp -h 172.22.37.1) || (4 a G 172.22.36 >/dev/null && ncmpcpp -h 172.22.36.117) || echo "Not sure which MPD to use"'

# Lock the screen and suspend to RAM.
alias susp='i3lock -i ~/Bilder/ramona_flowers_1280.png -t && sudo sh -c "echo mem > /sys/power/state"'

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
chpwd_functions=(${chpwd_functions} git_branch_chdir)

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
chpwd_functions=(${chpwd_functions} cwd_to_urxvt)


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
# -demuxer +mpegpes: tell mplayer what kind of stream this is
	mplayer -noidx -framedrop -vf kerndeint -prefer-ipv4 -cache 2048 -demuxer +mpegpes http://tv:7000/${1}
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
	if [ $USER = "michael" ]; then
		_main_fmt="%m"
	else
		_main_fmt="%n@%m"
	fi

	if [ $lvl -ge 2 ] ; then
		PROMPT="%K{cyan}%F{black}$_main_fmt%k%f ${fg_green}%~${fg_no_colour} \$(get_git_prompt_info)$lvl $ "
	else
		PROMPT="%K{cyan}%F{black}$_main_fmt%k%f ${fg_green}%~${fg_no_colour} \$(get_git_prompt_info)$ "
	fi
}

setup_prompt

# Use VI-mode for entering commands
setopt VI

# Export language
#export LANG=en_US.utf8
export LC_CTYPE=de_DE.UTF-8
export LC_COLLATE=de_DE.UTF-8
export LC_TIME=en_DK.UTF-8
export LC_NUMERIC=de_DE.UTF-8
export LC_MONETARY=de_DE.UTF-8
export LC_MESSAGES=C
export LC_PAPER=de_DE.UTF-8
export LC_NAME=de_DE.UTF-8
export LC_ADDRESS=de_DE.UTF-8
export LC_TELEPHONE=de_DE.UTF-8
export LC_MEASUREMENT=de_DE.UTF-8
export LC_IDENTIFICATION=de_DE.UTF-8

# Expand path to /usr/sbin and /sbin (because i know which binaries i can call)
export PATH=~/go/bin:~/.local/bin:~/.bin:$PATH:/usr/sbin:/sbin
export GOPATH=~/gocode

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
setterm -blength 0 # Don't REALLY beep
zle-line-init () { echo -n "$bellchar" }
zle -N zle-line-init

# Show all processes when completing kill/killall and enable menu mode
zstyle ':completion:*:processes' command 'ps f -N --ppid=$(pidof kthreadd) --pid=$(pidof kthreadd)'
zstyle ':completion:*:processes-names' command 'ps -aeo comm='
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:*:killall:*' menu yes select

# Directory specific configuration
function chpwd_profiles() {
	if [[ ${PWD} =~ "$HOME/i3(|/|/*)" ]]
	then
		alias m='make -j8'
	else
		alias m='make'
	fi

	if [[ ${PWD} =~ "$HOME/DPKG(|/|/*)" ]]
	then
		export GIT_AUTHOR_EMAIL="stapelberg@debian.org"
	else
		unset GIT_AUTHOR_EMAIL
	fi
}
chpwd_functions=( ${chpwd_functions} chpwd_profiles )
# Call this function before the first chpwd. This is necessary to get correct
# aliases in subshells (VIM’s :sh for example).
chpwd_profiles

# Initialize SSH completion only with hosts in my ~/.ssh/config, but especially with the aliases
# I gave them (and the full host names).
[ -e "$HOME/.ssh/config" ] && zstyle ':completion:*:complete:ssh:*:hosts' hosts $(sed -n "s/^[ \\t]*Host\(name\|\) \(.*\)/\\2/p" $HOME/.ssh/config | uniq)

[ -e "$HOME/.zshrc_host" ] && source ~/.zshrc_host

cfgfiles=$(dirname $(readlink ~/.zshrc))
# If the configfiles are in a git repository, update if it’s older than one hour
find $cfgfiles -maxdepth 1 -name .git -mmin +60 -execdir ./update.sh \;

# load RVM
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"

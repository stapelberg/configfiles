# Save 2000 lines of history
HISTSIZE=2000
HISTFILE=~/.zsh_history
SAVEHIST=2000
# Do not save duplicate entries
setopt HIST_IGNORE_DUPS
setopt INC_APPEND_HISTORY
setopt COMPLETE_IN_WORD

setopt short_loops

export EDITOR='vim'
export VISUAL='vim'
export PAGER='less'
export GTK_IM_MODULE=xim
# prefer library/system calls/programming manuals
export MANSECT="8:3:2:3posix:3pm:3perl:1:n:l:5:4:9:6:7"

bindkey -M viins >&-
bindkey -M viins "^[[A" up-line-or-history
bindkey -M viins "^[[B" down-line-or-history
bindkey -M viins "^F" push-line

# NO BEEPING!
setopt no_BEEP

# Don't display an error if there are no matches, I know what I am doing
setopt no_NOMATCH

# Leave processes open when closing a shell with background processes
setopt no_HUP

# Skip .o-files when completing for vi
fignore=(.o)

# Nicer output of ls
alias ls='ls --color=auto'
alias ll='ls -hl'
alias l='ll'
alias rt='ls -hltr'
alias L='dpkg -L'
# Find files in current folder
function f() {
	q="*$1*"
	find . -iname $q
}

alias bc='bc -ql'

alias ü='cd /home/michael/Uni/S4/Uebungen'

alias cal='cal -y'

# More passwords, faster!
alias pw='pwgen -s 23 1'

# Color output of man
alias man="TERMINFO=~/.terminfo/ LESS=C TERM=mostlike PAGER=less man"

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
# one-key-ssh-aliases
alias s='ssh ircd'

alias nh='unset HISTFILE'

alias acs='apt-cache search'
alias acsn='apt-cache search --names-only'
alias acp='apt-cache policy'
function agi { sudo apt-get install $* && rehash }
_da() { _deb_packages uninstalled; }
alias agu='sudo apt-get update'
alias cupt-upgrade='sudo cupt -o debug::resolver=1 -i -V -D -R full-upgrade i3-wm/installed sudo/installed 2>&1 | tee /tmp/cupt.log'

function agr { sudo apt-get remove $* && rehash }

alias smi='sudo make install'

alias g='git'

alias yt='clive --stream-exec="mplayer %i;" --stream 20 '

# Go into suspend-to-ram (we need to start echo in a subshell to redirect its output)
# Also, we lock the screen before and fix the brightness afterwards
alias susp='i3lock -i /home/michael/i3lock/To_the_Field_of_Dreams_by_justMANGO.xpm && sudo sh -c "echo mem > /sys/power/state && echo down > /proc/acpi/ibm/brightness && echo up > /proc/acpi/ibm/brightness" && sudo ifdown --force wlan0 && sudo ifup wlan0' 

function set_termtitle() {
	# escape '%' chars in $1, make nonprintables visible
	a=${(V)1//\%/\%\%}

	# Truncate command, and join lines.
	a=$(print -rPn "$a" | tr -d "\n\r")

	[ "$a" = "zsh" ] && { a=$(print -Pn "%~") }

	case $TERM in
	screen)
		# plain xterm title
		print -Pn "\e]2;$2: "
		print -rn "$a"
		print -n "\a"

		# screen title (in ^A")
		print -n "\ek"
		print -rn "$a"
		print -n "\e\\"

		# screen location
		print -Pn "\e_$2: "
		print -rn "$a"
		print -n "\e\\"
	;;
	xterm*|rxvt)
		# plain xterm title
		print -Pn "\e]2;$2: "
		print -rn "$a"
		print -n "\a"
	;;
	esac
}

function precmd() {
	set_termtitle "zsh" "%m"
}

function preexec() {
	set_termtitle "$1" "%m"
}

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

if [[ "$(hostname)" != "midna" && "$(hostname)" != "x200" ]]; then
	if [ $lvl -ge 2 ] ; then
		PROMPT="${fg_light_blue}%n${fg_no_colour}@%m ${fg_green}%~${fg_no_colour} $lvl $ "
	else
		PROMPT="${fg_light_blue}%n${fg_no_colour}@%m ${fg_green}%~${fg_no_colour}$ "
	fi
else
	if [ $lvl -ge 2 ] ; then
		PROMPT="${fg_light_blue}%n${fg_no_colour} ${fg_green}%~${fg_no_colour} $lvl$ "
	else
		PROMPT="${fg_light_blue}%n${fg_no_colour} ${fg_green}%~${fg_no_colour}$ "
	fi
fi

# Use VI-mode for entering commands
setopt VI

# Export language
#export LANG=en_US.utf8
export LC_CTYPE=de_DE.UTF-8
export LC_COLLATE=de_DE.UTF-8
export LC_TIME=de_DE.UTF-8
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
export PATH=$PATH:/usr/sbin:/sbin

# Initialize completion
autoload compinit
compinit -C
compdef _da agi

# Initialize SSH completion only with hosts in my ~/.ssh/config, but especially with the aliases
# I gave them (and the full host names).
[ -e "$HOME/.ssh/config" ] && zstyle ':completion:*:complete:ssh:*:hosts' hosts $(sed -n "s/^[ \\t]*Host\(name\|\) \(.*\)/\\2/p" $HOME/.ssh/config | uniq)

[ -e "$HOME/.zshrc_host" ] && source ~/.zshrc_host

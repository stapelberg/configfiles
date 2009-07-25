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

bindkey -M viins >&-
bindkey -M viins "^[[A" up-line-or-history
bindkey -M viins "^[[B" down-line-or-history

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
alias rt='ls -hltr'

alias bc='bc -ql'

alias ü='cd /home/michael/Uni/S2/Uebungen'

alias gitize='git init && git add . && git commit -a'

# More passwords, faster!
alias pw='pwgen -s 23 1'

# Color output of man
alias man="TERMINFO=~/.terminfo/ LESS=C TERM=mostlike PAGER=less man"

# Open terminal in ISO-8859-15-Mode
alias iso='LANG=en_US.iso885915 LC_ALL=en_US.iso885915 urxvt'

alias ripcd='cdparanoia -Bs 1-'
alias cam="mplayer -v tv:// -tv device=/dev/video0:driver=v4l2:outfmt=yuy2"
# wiipdf with the ID of my primary wiimote
alias wp="wiipdf 00:19:1D:93:CA:EB "

alias asdf='/home/michael/toggle_layout.sh'
alias uiae='/home/michael/toggle_layout.sh'

alias update-mirror='debmirror --getcontents --passive --verbose --progress --nosource --host=ftp.de.debian.org --dist=squeeze,sid --arch=amd64 debian && sudo apt-get update'
alias update-mirror-multimedia='debmirror debian-multimedia --getcontents --passive --ignore-small-errors --verbose --progress --ignore-release-gpg --host=www.debian-multimedia.org --dist=stable,testing --arch=amd64 --root=/ --method=http --section=main'

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

alias sd='svn diff --diff-cmd=colordiff | less -R'

alias acs='apt-cache search'
alias acp='apt-cache policy'
alias agi='sudo apt-get install'
alias agu='sudo apt-get update'
alias agr='sudo apt-get remove'

alias smi='sudo make install'

alias g='git'

alias yt='clive --stream-exec="mplayer %i;" --stream 20 '

# Go into suspend-to-ram (we need to start echo in a subshell to redirect its output)
# Also, we lock the screen before and fix the brightness afterwards
alias susp='i3lock -i /home/michael/i3lock/To_the_Field_of_Dreams_by_justMANGO.xpm && sudo sh -c "echo mem > /sys/power/state && echo down > /proc/acpi/ibm/brightness && echo up > /proc/acpi/ibm/brightness"' 

alias blauzahn='sudo sh -c "echo enable > /proc/acpi/ibm/bluetooth && /etc/init.d/bluetooth restart"'
alias blauzahn-aus='sudo sh -c "/etc/init.d/bluetooth stop && echo disable > /proc/acpi/ibm/bluetooth"'

# Play random movie
alias mprnd='files=(*); let "r = $RANDOM % ${#files}"; ([ -f ${files[$r]} ] && mplayer ${files[$r]}) || ([ -d ${files[$r]} ] && mplayer ${files[$r]}/*{avi,mpg} )'

function set_termtitle() {
	# escape '%' chars in $1, make nonprintables visible
	a=${(V)1//\%/\%\%}

	# Truncate command, and join lines.
	a=$(print -Pn "$a" | tr -d "\n")

	case $TERM in
	screen)
		print -Pn "\e]2;$2: $a\a" # plain xterm title
		print -Pn "\ek$a\e\\"      # screen title (in ^A")
		print -Pn "\e_$2: $a\e\\"   # screen location
	;;
	xterm*|rxvt)
		print -Pn "\e]2;$2: $a\a" # plain xterm title
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

# Don't use: broken xpdf currently
function new_pdf {
	FN=$(mktemp -u)
	nohup xpdf -title xpdf.$FN -fullscreen -remote $FN $* 1>&- 2>&- &!
	for (( i=0; i < 20; i=i+1 ))
		fgrep -q xpdf.$FN /mnt/wmii/client/0x*/label && \
		{ sleep 0.1; xpdf -remote $FN -exec zoomFitWidth; break } \
		|| sleep 0.1
}

function pdf {
	FN=$(mktemp -u)
	nohup xpdf -title xpdf.$FN $* 1>&- 2>&- &!
	(cd /mnt/wmii && i=0 && while [ $((i=i+1)) -lt 20 ]; do
		sleep 0.1
		C=$(fgrep -l xpdf.$FN client/0x*/label)
		[ $? -eq 0 ] && { echo "Fullscreen toggle" >> ${C/label/ctl}; break }
	done)
}

function latest_pdf {
	LATEST=${1}/$(ls -t ${1} | grep '.pdf' -m 1)
	pdf $LATEST
}

function alda {
	latest_pdf "/home/michael/Uni/S2/Uebungen/alda"
}

function info2 {
	latest_pdf "/home/michael/Uni/S2/Uebungen/info2"
}

function ana2 {
	latest_pdf "/home/michael/Uni/S2/Uebungen/ana2"
}

# Replacement for ps uax | grep $foo
function psof {
	PIDS=$(pidof ${1})
	[ -z "${PIDS}" ] && { echo "No such process"; return }
	ps -p "$PIDS" u
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

# Compresses all *.JPG and *.jpg files in the current directory and adds a nice border around
# Also creates thumbnails with very little size
function webpic {
	NAME=${1}
	c=0
	mkdir -p web/thumbs
	# Resize pictures
	for i in $(find . -iname "*.jpg"); do
		if [ $c -lt 10 ]; then
			rc=0${c}
		else
			rc=${c}
		fi
		[ ! -f web/${NAME}_${c}.jpg ] && {
			convert -resize 800x600\> $i web/${NAME}_${rc}.jpg -bordercolor white -border 6 -bordercolor grey60 -border 1 -background none -background black \( +clone -shadow 60x4+4+4 \) +swap -background white -flatten -depth 8 web/${NAME}_${rc}.jpg
		}
		c=$((c+1))
	done
	c=0
	# Generate thumbnails
	for i in $(find . -iname "*.jpg"); do
		if [ $c -lt 10 ]; then
			rc=0${c}
		else
			rc=${c}
		fi

		convert -resize 100x\> $i web/thumbs/${NAME}_${rc}.jpg
		c=$((c+1))
	done
}

# Same as webpic, but it doesn't add the borders and doesn't generate thumbs
function webresize {
	NAME=${1}
	c=0
	mkdir web
	# Resize pictures
	for i in $(find . -iname "*.jpg"); do
		if [ $c -lt 10 ]; then
			rc=0${c}
		else
			rc=${c}
		fi

		[ ! -f web/${NAME}_${rc}.jpg ] && convert -resize 800x600\> $i web/${NAME}_${rc}.jpg
		c=$((c+1))
	done
}

# Define prompt
fg_green=$'%{\e[1;32m%}'
fg_light_blue=$'%{\e[1;36m%}'
fg_light_silver=$'%{\e[1;37m%}'
fg_white=$'%{\e[1;37m%}'
fg_no_colour=$'%{\e[0m%}'
PROMPT="${fg_light_blue}%n${fg_no_colour} ${fg_green}%~${fg_no_colour}$ "

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

# Initialize SSH completion only with hosts in my ~/.ssh/config, but especially with the aliases
# I gave them (and the full host names).
[ -e "$HOME/.ssh/config" ] && zstyle ':completion:*:complete:ssh:*:hosts' hosts $(sed -n "s/^[ \\t]*Host\(name\|\) \(.*\)/\\2/p" $HOME/.ssh/config | uniq)

[ -e "$HOME/.zshrc_host" ] && source ~/.zshrc_host

# Set title
chpwd () { print -Pn "\e]0;%n@%m: %~\a" }
chpwd

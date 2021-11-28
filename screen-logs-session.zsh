#!/bin/zsh

tmp=$(mktemp)
cat ~/.screenrc > "${tmp}"
cat >> "${tmp}" <<'EOT'
screen -t 'midna syslog' /home/michael/configfiles/screenrc-syslog.zsh
screen -t 'all mqtt' /home/michael/configfiles/screenrc-mqtt.zsh
screen -t 'router7 serial' /home/michael/configfiles/screenrc-router7-serial.zsh
EOT
screen -c "${tmp}"
rm "${tmp}"

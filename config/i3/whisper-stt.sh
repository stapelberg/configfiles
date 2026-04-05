#!/usr/bin/env bash
# Toggle speech-to-text using whisper-cpp via systemd.
# i3 keybinding:
#   bindsym $mod+comma exec --no-startup-id whisper-stt.sh toggle

set -e

NOTIFYIDFILE="/tmp/whisper-stt.notifyid"

notify() {
    local urgency="$1" title="$2" body="${3:-}"
    local replace_args=()
    if [ -f "$NOTIFYIDFILE" ]; then
        replace_args=(-r "$(cat "$NOTIFYIDFILE")")
    fi
    local id
    id=$(dunstify --printid --timeout 999999 "${replace_args[@]}" -u "$urgency" "$title" "$body")
    echo "$id" > "$NOTIFYIDFILE"
}

case "${1}" in
    toggle)
        if systemctl --user is-active --quiet whisper-stt.service; then
            # Recording — stop it (triggers transcription)
            systemctl --user stop --no-block whisper-stt.service
        elif systemctl --user show -p SubState --value whisper-stt.service | grep -q deactivating; then
            # Transcription in progress — kill it
            systemctl --user kill whisper-stt.service
            sleep 0.3
            systemctl --user kill --signal=SIGKILL whisper-stt.service 2>/dev/null
            systemctl --user reset-failed whisper-stt.service 2>/dev/null
            notify low "Whisper" "Cancelled"
        else
            # Idle — start recording
            rm -f /tmp/whisper-stt.wav
            systemctl --user start whisper-stt.service
            notify low "Whisper" "Recording..."
        fi
        ;;
    *) echo "Usage: $0 toggle" >&2; exit 1 ;;
esac

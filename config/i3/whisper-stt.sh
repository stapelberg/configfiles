#!/usr/bin/env bash
# Toggle speech-to-text using whisper-cpp.
# i3 keybinding:
#   bindsym $mod+comma exec whisper-stt.sh toggle

set -e

PIDFILE="/tmp/whisper-stt.pid"
WAVFILE="/tmp/whisper-stt.wav"
LOGFILE="/tmp/whisper-stt.log"
NOTIFYIDFILE="/tmp/whisper-stt.notifyid"
MODEL="${WHISPER_MODEL:-$HOME/.local/share/whisper/ggml-base.bin}"

log() {
    echo "$(date '+%H:%M:%S.%3N') [$$] $*" >> "$LOGFILE"
}

notify() {
    local urgency="$1" title="$2" body="${3:-}"
    local replace_args=()
    if [ -f "$NOTIFYIDFILE" ]; then
        replace_args=(-r "$(cat "$NOTIFYIDFILE")")
    fi
    local id
    id=$(dunstify --printid --timeout 999999 "${replace_args[@]}" -u "$urgency" "$title" "$body")
    echo "$id" > "$NOTIFYIDFILE"
    log "notify: id=$id urgency=$urgency title='$title' body='$body'"
}

notify_close() {
    if [ -f "$NOTIFYIDFILE" ]; then
        dunstify -C "$(cat "$NOTIFYIDFILE")" 2>/dev/null || true
        rm -f "$NOTIFYIDFILE"
    fi
}

start() {
    log "start: begin"

    # Kill any leftover recording
    if [ -f "$PIDFILE" ]; then
        log "start: killing leftover arecord pid=$(cat "$PIDFILE")"
        kill "$(cat "$PIDFILE")" 2>/dev/null || true
        rm -f "$PIDFILE"
    fi
    rm -f "$WAVFILE"

    notify low "Whisper" "Recording..."

    arecord -f S16_LE -r 16000 -c 1 -t wav "$WAVFILE" &
    ARECORD_PID=$!
    echo "$ARECORD_PID" > "$PIDFILE"
    log "start: arecord running pid=$ARECORD_PID"

    # Keep the script alive so arecord isn't orphaned
    wait "$ARECORD_PID" 2>/dev/null || true
    log "start: arecord exited"
}

stop() {
    log "stop: begin"

    if [ ! -f "$PIDFILE" ]; then
        log "stop: no pidfile, aborting"
        exit 0
    fi

    PID=$(cat "$PIDFILE")
    log "stop: killing arecord pid=$PID"
    kill "$PID" 2>/dev/null || true
    rm -f "$PIDFILE"

    notify low "Whisper" "Transcribing..."

    # Give arecord a moment to flush the WAV header
    sleep 0.1

    if [ ! -f "$WAVFILE" ]; then
        log "stop: no wav file, aborting"
        notify critical "Whisper" "No audio recorded"
        exit 0
    fi

    WAV_SIZE=$(stat -c%s "$WAVFILE" 2>/dev/null || echo 0)
    log "stop: wav size=$WAV_SIZE bytes"

    if [ "$WAV_SIZE" -lt 1000 ]; then
        log "stop: wav too small, aborting"
        rm -f "$WAVFILE"
        notify critical "Whisper" "Recording too short"
        exit 0
    fi

    log "stop: running whisper-cli model=$MODEL"
    WHISPER_START=$(date +%s%3N)

    text=$(whisper-cli \
        -m "$MODEL" \
        -f "$WAVFILE" \
        -l en \
        --no-timestamps \
        -nt \
        2>>"$LOGFILE") || {
        log "stop: whisper-cli failed exit=$?"
        rm -f "$WAVFILE"
        notify critical "Whisper" "Transcription failed"
        exit 1
    }

    WHISPER_END=$(date +%s%3N)
    log "stop: whisper-cli took $((WHISPER_END - WHISPER_START))ms"
    rm -f "$WAVFILE"

    # Strip leading/trailing whitespace
    text=$(echo "$text" | sed 's/^[[:space:]]*//;s/[[:space:]]*$//')

    log "stop: text='$text' (${#text} chars)"

    if [ -n "$text" ]; then
        log "stop: typing via xdotool"
	notify_close
        xdotool type -- "$text"
        log "stop: done"
    else
        log "stop: empty text, nothing to type"
        notify low "Whisper" "(no speech detected)"
    fi
}

toggle() {
    if [ -f "$PIDFILE" ] && kill -0 "$(cat "$PIDFILE")" 2>/dev/null; then
        stop
    else
        start
    fi
}

case "${1}" in
    start) start ;;
    stop) stop ;;
    toggle) toggle ;;
    *) echo "Usage: $0 {start|stop|toggle}" >&2; exit 1 ;;
esac

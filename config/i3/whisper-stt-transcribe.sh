#!/usr/bin/env bash
# Called by whisper-stt.service ExecStopPost to transcribe and paste.
#
# Uses whisper.cpp (whisper-cli) for inference. We benchmarked faster-whisper
# (CTranslate2) on 2026-04-05 and it was ~1.5x SLOWER than whisper.cpp on our
# hardware (Ryzen 9 9950X3D) because whisper.cpp uses the AVX-512 zen4 backend
# while CTranslate2 uses OpenBLAS. The "5x faster" claims are vs the original
# Python whisper, not vs whisper.cpp. Accuracy was identical.
#
# Parakeet V3 (NVIDIA NeMo, via sherpa-onnx) is a potential future upgrade for
# better accuracy, but sherpa-onnx is not in nixpkgs yet.

WAVFILE="/tmp/whisper-stt.wav"
WAVARCHIVE="$HOME/stt/$(date '+%Y-%m-%d')"
LOGFILE="/tmp/whisper-stt.log"
NOTIFYIDFILE="/tmp/whisper-stt.notifyid"
MODEL="${WHISPER_MODEL:-$HOME/.local/share/whisper/ggml-small.bin}"

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

trap 'notify_close; log "transcribe: killed"; exit 1' TERM INT HUP

log "transcribe: begin"

notify low "Whisper" "Transcribing..."

# Give arecord a moment to flush the WAV header
sleep 0.1

if [ ! -f "$WAVFILE" ]; then
    log "transcribe: no wav file"
    notify critical "Whisper" "No audio recorded"
    exit 0
fi

WAV_SIZE=$(stat -c%s "$WAVFILE" 2>/dev/null || echo 0)
log "transcribe: wav size=$WAV_SIZE bytes"

if [ "$WAV_SIZE" -lt 1000 ]; then
    log "transcribe: wav too small"
    rm -f "$WAVFILE"
    notify critical "Whisper" "Recording too short"
    exit 0
fi

log "transcribe: running whisper-cli model=$MODEL"
WHISPER_START=$(date +%s%3N)

text=$(whisper-cli \
    -m "$MODEL" \
    -f "$WAVFILE" \
    -l auto \
    --no-timestamps \
    -nt \
    -ng \
    -t 16 \
    -bs 1 \
    -bo 1 \
    2>>"$LOGFILE") || {
    log "transcribe: whisper-cli failed exit=$?"
    rm -f "$WAVFILE"
    notify critical "Whisper" "Transcription failed"
    exit 1
}

WHISPER_END=$(date +%s%3N)
log "transcribe: whisper-cli took $((WHISPER_END - WHISPER_START))ms"

# Strip leading/trailing whitespace (including newlines from whisper-cli)
text="${text#"${text%%[![:space:]]*}"}"
text="${text%"${text##*[![:space:]]}"}"

log "transcribe: text='$text' (${#text} chars)"

# Archive WAV + metadata for retroactive model evaluation
mkdir -p "$WAVARCHIVE"
LAST=$(ls -d "$WAVARCHIVE"/recording-*/ 2>/dev/null | sort | tail -1 | grep -o '[0-9]\{5\}')
NEXT=$(printf '%05d' $(( 10#${LAST:-0} + 1 )))
MODELNAME=$(basename "$MODEL" .bin)
RECDIR="$WAVARCHIVE/recording-$NEXT"
mkdir -p "$RECDIR"
cp "$WAVFILE" "$RECDIR/mic-raw-capture.wav"
cat > "$RECDIR/$MODELNAME.txt" <<EOF
duration_ms: $((WHISPER_END - WHISPER_START))
text: $text
EOF
log "transcribe: archived $RECDIR/mic-raw-capture.wav"
rm -f "$WAVFILE"

if [ -n "$text" ]; then
    log "transcribe: pasting via xclip+shift+Insert"
    notify_close
    printf '%s' "$text" | xclip -selection primary
    xdotool key --clearmodifiers shift+Insert
    sleep 0.2
    log "transcribe: done"
else
    log "transcribe: empty text"
    notify low "Whisper" "(no speech detected)"
fi

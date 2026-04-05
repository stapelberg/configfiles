#!/usr/bin/env bash
# Evaluate all whisper models against all recordings, skipping existing results.
# Usage: whisper-stt-eval.sh [~/stt]

set -e

STTDIR="${1:-$HOME/stt}"
MODELS=(
    "$HOME/.local/share/whisper/ggml-base.bin"
    "$HOME/.local/share/whisper/ggml-small.bin"
    "$HOME/.local/share/whisper/ggml-small-q5_1.bin"
    "$HOME/.local/share/whisper/ggml-medium-q5_0.bin"
    "$HOME/.local/share/whisper/ggml-large-v2-q5_0.bin"
    "$HOME/.local/share/whisper/ggml-large-v3-turbo-q5_0.bin"
    "$HOME/nobackup/ggml-large-v3-turbo-q8_0.bin"
)

for recdir in "$STTDIR"/*/recording-*/; do
    [ -d "$recdir" ] || continue
    wav="$recdir/mic-raw-capture.wav"
    [ -f "$wav" ] || continue

    for model in "${MODELS[@]}"; do
        [ -f "$model" ] || continue
        modelname=$(basename "$model" .bin)
        txtfile="$recdir/$modelname.txt"

        # Skip if already evaluated
        [ -f "$txtfile" ] && continue

        echo "=== $(basename "$(dirname "$recdir")")/$(basename "$recdir") + $modelname ==="

        start_ms=$(date +%s%3N)
        text=$(whisper-cli \
            -m "$model" \
            -f "$wav" \
            -l auto \
            --no-timestamps \
            -nt \
            -ng \
            -t 16 \
            -bs 1 \
            -bo 1 \
            2>/dev/null) || {
            echo "  FAILED (exit=$?)"
            continue
        }
        end_ms=$(date +%s%3N)
        duration=$(( end_ms - start_ms ))

        # Strip leading/trailing whitespace
        text="${text#"${text%%[![:space:]]*}"}"
        text="${text%"${text##*[![:space:]]}"}"

        cat > "$txtfile" <<EOF
duration_ms: $duration
text: $text
EOF
        echo "  ${duration}ms: $text"
    done
done

echo "Done."

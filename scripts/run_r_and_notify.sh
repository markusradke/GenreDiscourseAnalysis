#!/bin/bash

# Check if a script path is provided
if [ $# -eq 0 ]; then
    echo "Usage: $0 <path_to_Rscript> [email_recipient]"
    exit 1
fi

# Script path is the first argument
RSCRIPT_PATH="$1"

# Optional email recipient (default if not provided)
RECIPIENT="${2:-mail@markus-radke.de}"

SUBJECT_SUCCESS="Rscript finished: $(basename "$RSCRIPT_PATH")"
SUBJECT_FAIL="Rscript interrupted or failed: $(basename "$RSCRIPT_PATH")"
BODY_SUCCESS="Rscript $(basename "$RSCRIPT_PATH") completed successfully on $(hostname) at $(date)."
BODY_FAIL="Rscript $(basename "$RSCRIPT_PATH") stopped (exit code $?) on $(hostname) at $(date)."

# Validate script exists
if [ ! -f "$RSCRIPT_PATH" ]; then
    echo "Error: R script not found at $RSCRIPT_PATH"
    exit 1
fi

# cleanup function called on exit
_on_exit() {
    code=$?
    if [ $code -eq 0 ]; then
        echo "$BODY_SUCCESS" | mail -r markus@hendrix.ak.tu-berlin.de -s "$SUBJECT_SUCCESS" "$RECIPIENT"
    else
        echo "$BODY_FAIL" | mail -r markus@hendrix.ak.tu-berlin.de -s "$SUBJECT_FAIL" "$RECIPIENT"
    fi
}
trap _on_exit EXIT INT TERM

# run the R script
Rscript "$RSCRIPT_PATH"
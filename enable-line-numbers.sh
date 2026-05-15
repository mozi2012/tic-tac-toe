#!/bin/bash
# Script to enable global line numbers in Emacs init.el

INIT_FILE="$HOME/.emacs.d/init.el"
CONFIG_LINE="(global-display-line-numbers-mode t)"

if [ -f "$INIT_FILE" ]; then
    if grep -Fq "$CONFIG_LINE" "$INIT_FILE"; then
        echo "Line numbers configuration already exists in $INIT_FILE"
    else
        echo "" >> "$INIT_FILE"
        echo "$CONFIG_LINE" >> "$INIT_FILE"
        echo "Successfully added line numbers configuration to $INIT_FILE"
    fi
else
    echo "Error: Emacs init file not found at $INIT_FILE"
    exit 1
fi

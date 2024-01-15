#! /bin/bash
session="$(tmux display-message -p '#S')"
if [[ ! "$session" == "0" ]]; then
    echo "$session |"
fi

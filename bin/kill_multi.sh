#!/bin/bash
log_file="$HOME/.local/share/phylotrace/logs/script_log.txt"
blat_multi="bin/multi_typing.sh"
automatic_typing="multi_eval.R"

# Function to log messages
log_message() {
    echo "$(date +"%Y-%m-%d %H:%M:%S") - $1" >> "$log_file"
}

# Function to safely kill a process if found
kill_if_running() {
    local process_name="$1"
    local pid
    pid=$(pgrep -f "$process_name")

    if [[ -n "$pid" ]]; then
        kill "$pid"
        log_message "Killed process $pid for $process_name"
    fi
}

# Kill processes if they exist
kill_if_running "$blat_multi"
kill_if_running "$automatic_typing"

# Kill all Perl parallel jobs if any exist
if pgrep -x "perl" > /dev/null; then
    killall -TERM perl
    log_message "Killed all running Perl processes"
fi

# Reset log file
echo 0 > "$log_file"

#!/bin/bash
SCRIPT_DIR=/home/filip/PhyloTrace
CONDA_PATH=/home/filip/miniconda3/bin/conda
# Activate conda
eval "$("$CONDA_PATH" shell.bash hook)"
conda activate PhyloTrace

# The app dir must also be R's working directory: .Rprofile (box.path, the
# default browser option, ...) is only sourced from the process's cwd at
# startup, not from wherever this script happens to be invoked from.
cd "$SCRIPT_DIR" || exit 1
mkdir -p "$HOME/.local/share/phylotrace/logs"

# R_BROWSER picks the browser shiny opens on launch. Respect a caller-set
# value; on WSL default to wslview (xdg-open doesn't work there); everywhere
# else leave it unset and let .Rprofile's own xdg-open default apply.
if [ -z "$R_BROWSER" ] && [[ $(uname -r) == *"microsoft"* ]]; then
    export R_BROWSER=wslview
fi

Rscript -e "shiny::runApp(launch.browser=TRUE)" > "$HOME/.local/share/phylotrace/logs/last_session.log" 2>&1

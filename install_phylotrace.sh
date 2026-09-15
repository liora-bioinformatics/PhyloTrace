#!/bin/bash
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

echo -e "\e[36m"
cat << 'BANNER'
  ____  _           _       _____
 |  _ \| |__  _   _| | ___ |_   _| __ __ _  ___ ___
 | |_) | '_ \| | | | |/ _ \  | || '__/ _` |/ __/ _ \
 |  __/| | | | |_| | | (_) | | || | | (_| | (_|  __/
 |_|   |_| |_|\__, |_|\___/  |_||_|  \__,_|\___\___|
              |___/
BANNER
echo -e "\e[0m"
echo "cgMLST-based bacterial pathogen monitoring -- installer"
echo ""

# Locate conda: prefer whatever is already on PATH, otherwise fall back to the
# two locations README's own install instructions produce (`conda init`
# writes its hook into ~/.bashrc, but that's only sourced by interactive
# shells -- this script may run non-interactively).
if ! command -v conda >/dev/null 2>&1; then
    for candidate in "$HOME/miniconda3" "$HOME/anaconda3"; do
        if [ -f "$candidate/etc/profile.d/conda.sh" ]; then
            # shellcheck disable=SC1091
            . "$candidate/etc/profile.d/conda.sh"
            break
        fi
    done
fi

if ! command -v conda >/dev/null 2>&1; then
    if [ -e "$HOME/miniconda3" ]; then
        echo -e "\e[31mError: Conda was not found, and $HOME/miniconda3 already exists but doesn't look like a working conda install.\e[0m"
        echo -e "\e[31mPlease check that directory manually -- it will not be touched or overwritten.\e[0m"
        exit 1
    fi

    if ! command -v curl >/dev/null 2>&1 && ! command -v wget >/dev/null 2>&1; then
        echo -e "\e[31mError: Neither curl nor wget is installed. Please install one of them first:\e[0m"
        echo "To install wget:"
        echo "  Ubuntu/Debian:  sudo apt install wget"
        echo "  Arch Linux:     sudo pacman -S wget"
        echo "  RHEL/Fedora:    sudo dnf install wget"
        echo "To install curl:"
        echo "  Ubuntu/Debian:  sudo apt install curl"
        echo "  Arch Linux:     sudo pacman -S curl"
        echo "  RHEL/Fedora:    sudo dnf install curl"
        exit 1
    fi

    echo "Conda not found. Installing Miniconda ..."
    MINICONDA_URL="https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-$(uname -m).sh"
    if command -v curl >/dev/null 2>&1; then
        curl -fsSL "$MINICONDA_URL" -o "$HOME/miniconda.sh"
    else
        wget -q "$MINICONDA_URL" -O "$HOME/miniconda.sh"
    fi
    if [ ! -s "$HOME/miniconda.sh" ]; then
        echo -e "\e[31mError: Failed to download the Miniconda installer.\e[0m"
        exit 1
    fi
    # No -b: run interactively so the user reads and accepts the license/ToS themselves.
    bash "$HOME/miniconda.sh"
    INSTALL_STATUS=$?
    rm "$HOME/miniconda.sh"
    if [ $INSTALL_STATUS -ne 0 ] || [ ! -f "$HOME/miniconda3/etc/profile.d/conda.sh" ]; then
        echo -e "\e[31mError: Miniconda installation was not completed (license declined or install failed).\e[0m"
        exit 1
    fi

    # shellcheck disable=SC1091
    . "$HOME/miniconda3/etc/profile.d/conda.sh"
    SHELL_NAME="$(basename "${SHELL:-bash}")"
    conda init "$SHELL_NAME" >/dev/null 2>&1 || conda init bash
    conda config --set auto_activate false

    echo ""
    echo -e "\e[33mMiniconda was just installed.\e[0m"
fi

CONDA_PATH="$(conda info --base)/bin/conda"
eval "$("$CONDA_PATH" shell.bash hook)"

echo "Conda detected. Continuing script..."
# Clean cache
conda clean --all -y

if conda env list | grep -q "PhyloTrace"; then
  echo "Environment PhyloTrace already exists. Updating the environment..."
  conda env update -f environment.yml
else
  echo "Environment PhyloTrace does not exist. Creating the environment..."
  conda create -f environment.yml
fi

if [ $? -ne 0 ]; then
    echo -e "\e[31mError: Setting up the PhyloTrace conda environment failed (see above -- a declined channel Terms of Service can cause this).\e[0m"
    exit 1
fi

conda activate PhyloTrace

# Generate PhyloTrace Desktop Entry
cat > PhyloTrace.desktop << EOF
[Desktop Entry]
Name=PhyloTrace
Exec=$SCRIPT_DIR/run_phylotrace.sh
Icon=PhyloTrace_flat
Terminal=false
Type=Application
Categories=Education
EOF

# Generate PhyloTrace run script
cat > run_phylotrace.sh << EOF
#!/bin/bash
SCRIPT_DIR=$SCRIPT_DIR
CONDA_PATH=$CONDA_PATH
EOF

cat >> run_phylotrace.sh << 'EOF'
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
EOF

# Setting up the Desktop Icon
mkdir -p "$HOME/.local/share/applications"
mkdir -p "$HOME/.local/share/icons/hicolor"/{48x48,64x64,256x256,512x512,scalable}/apps
mkdir -p "$HOME/.local/share/phylotrace/logs"
mv "$SCRIPT_DIR/PhyloTrace.desktop" "$HOME/.local/share/applications"
# Copy icons to appropriate sizes
cp "$SCRIPT_DIR/app/static/images/PhyloTrace_flat_48.png" "$HOME/.local/share/icons/hicolor/48x48/apps/PhyloTrace_flat.png"
cp "$SCRIPT_DIR/app/static/images/PhyloTrace_flat_64.png" "$HOME/.local/share/icons/hicolor/64x64/apps/PhyloTrace_flat.png"
cp "$SCRIPT_DIR/app/static/images/PhyloTrace_flat_256.png" "$HOME/.local/share/icons/hicolor/256x256/apps/PhyloTrace_flat.png"
cp "$SCRIPT_DIR/app/static/images/PhyloTrace_flat_512.png" "$HOME/.local/share/icons/hicolor/512x512/apps/PhyloTrace_flat.png"
# Use 512x512 for scalable/apps
cp "$SCRIPT_DIR/app/static/images/PhyloTrace_flat_512.png" "$HOME/.local/share/icons/hicolor/scalable/apps/PhyloTrace_flat.png"
# Update icon cache
gtk-update-icon-cache -f "$HOME/.local/share/icons/hicolor/" 2>/dev/null || true
chmod 700 "$SCRIPT_DIR/run_phylotrace.sh"

echo ""
echo "PhyloTrace installed. Launch it from your applications menu, or run:"
echo "  $SCRIPT_DIR/run_phylotrace.sh"

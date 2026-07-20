#!/bin/bash
set -e
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
VENV_DIR="$HOME/.local/share/phylotrace/venv"
PYMLST_VERSION="2.2.2"

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
echo "cgMLST-based bacterial pathogen monitoring -- installer (native: R/renv + Python/pip)"
echo ""

fail() {
    echo -e "\e[31mError: $1\e[0m"
    exit 1
}

# --- Prerequisite checks -----------------------------------------------------
# Nothing here is auto-installed with sudo: system packages are the user's call.
# renv (an R package) and the pymlst venv are user-space, so those steps below
# do install themselves.

command -v Rscript >/dev/null 2>&1 || fail "R was not found. Install it first, e.g.:
  Debian/Ubuntu/Mint: sudo apt install r-base
  Fedora:             sudo dnf install R
  Arch:                sudo pacman -S r"

command -v python3 >/dev/null 2>&1 || fail "Python 3 was not found. Install it first, e.g.:
  Debian/Ubuntu/Mint: sudo apt install python3
  Fedora:             sudo dnf install python3
  Arch:                sudo pacman -S python"

python3 -m pip --version >/dev/null 2>&1 || fail "pip was not found for python3. Install it first, e.g.:
  Debian/Ubuntu/Mint: sudo apt install python3-pip
  Fedora:             sudo dnf install python3-pip
  Arch:                sudo pacman -S python-pip"

python3 -m venv --help >/dev/null 2>&1 || fail "python3's venv module was not found. Install it first, e.g.:
  Debian/Ubuntu/Mint: sudo apt install python3-venv
  Fedora/Arch:        included with python3 already; check your installation"

echo "R, python3, pip and venv detected. Continuing..."

# wgMLST/claMLST (via the pymlst venv set up below) still need the BLAT and
# MAFFT binaries on PATH at typing time -- neither is a pip/renv package.
# Non-fatal here: the app itself reports a clear error if a typing run can't
# find them, so don't block installation over a tool that isn't needed until
# first use.
for tool in blat mafft; do
    if ! command -v "$tool" >/dev/null 2>&1; then
        echo -e "\e[33mWarning: '$tool' was not found on PATH. cgMLST typing needs it;"
        echo -e "install it before typing genomes (e.g. 'sudo apt install $tool' on"
        echo -e "Debian/Ubuntu/Mint, where available, or see https://pymlst.readthedocs.io).\e[0m"
    fi
done

# --- R packages, via renv -----------------------------------------------------
echo "Ensuring renv is installed..."
Rscript -e 'if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv", repos = "https://cloud.r-project.org")'

# ggtree/ggtreeExtra/treeio come from Bioconductor, not CRAN. Prime BiocManager
# and BiocVersion (the package that pins the Bioconductor release renv.lock
# expects) before renv::restore(), rather than leaving renv to resolve the
# Bioconductor repos on a bare R library.
echo "Ensuring BiocManager is installed..."
Rscript -e 'if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager", repos = "https://cloud.r-project.org")'

echo "Installing BiocVersion via BiocManager..."
Rscript -e 'BiocManager::install("BiocVersion", update = FALSE, ask = FALSE)'

echo "Restoring R packages from renv.lock (this can take a while on first run)..."
(cd "$SCRIPT_DIR" && Rscript -e 'renv::restore(prompt = FALSE)')

# --- pyMLST, via a dedicated venv --------------------------------------------
echo "Setting up the pyMLST virtual environment..."
mkdir -p "$(dirname "$VENV_DIR")"
if [ ! -f "$VENV_DIR/bin/activate" ]; then
    python3 -m venv "$VENV_DIR"
fi
"$VENV_DIR/bin/pip" install --upgrade pip --quiet
"$VENV_DIR/bin/pip" install --quiet "pymlst==$PYMLST_VERSION"

# --- Desktop integration ------------------------------------------------------
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
VENV_DIR=$VENV_DIR
EOF

cat >> run_phylotrace.sh << 'EOF'
# The app dir must also be R's working directory: .Rprofile (box.path, the
# default browser option, ...) is only sourced from the process's cwd at
# startup, not from wherever this script happens to be invoked from.
cd "$SCRIPT_DIR" || exit 1
mkdir -p "$HOME/.local/share/phylotrace/logs"

# Put pyMLST's wgMLST/claMLST commands (installed into the dedicated venv, not
# system-wide) on PATH for the R session to shell out to directly.
export PATH="$VENV_DIR/bin:$PATH"

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
mkdir -p "$HOME/.local/share/icons/hicolor"/{48x48,64x64,256x256,512x512}/apps
mkdir -p "$HOME/.local/share/phylotrace/logs"
mv "$SCRIPT_DIR/PhyloTrace.desktop" "$HOME/.local/share/applications"
# Copy icons to appropriate sizes
cp "$SCRIPT_DIR/app/static/images/PhyloTrace_flat_48.png" "$HOME/.local/share/icons/hicolor/48x48/apps/PhyloTrace_flat.png"
cp "$SCRIPT_DIR/app/static/images/PhyloTrace_flat_64.png" "$HOME/.local/share/icons/hicolor/64x64/apps/PhyloTrace_flat.png"
cp "$SCRIPT_DIR/app/static/images/PhyloTrace_flat_256.png" "$HOME/.local/share/icons/hicolor/256x256/apps/PhyloTrace_flat.png"
cp "$SCRIPT_DIR/app/static/images/PhyloTrace_flat_512.png" "$HOME/.local/share/icons/hicolor/512x512/apps/PhyloTrace_flat.png"
# Use 512x512 for scalable/apps
cp "$SCRIPT_DIR/app/static/images/PhyloTrace_flat_512.png" "$HOME/.local/share/icons/hicolor/scalable/apps/PhyloTrace_flat.png"
# Update index.theme
cat > "$HOME/.local/share/icons/hicolor/index.theme" << EOF
[Icon Theme]
Name=Hicolor
Comment=Fallback icon theme
Hidden=false
Directories=scalable/apps,48x48/apps,64x64/apps,256x256/apps,512x512/apps

[scalable/apps]
Size=128
Type=Scalable
MinSize=1
MaxSize=512
Context=Applications

[48x48/apps]
Size=48
Type=Fixed
Context=Applications

[64x64/apps]
Size=64
Type=Fixed
Context=Applications

[256x256/apps]
Size=256
Type=Fixed
Context=Applications

[512x512/apps]
Size=512
Type=Fixed
Context=Applications
EOF
# Update icon cache
gtk-update-icon-cache -f "$HOME/.local/share/icons/hicolor/" 2>/dev/null || true
chmod 700 "$SCRIPT_DIR/run_phylotrace.sh"

echo ""
echo "PhyloTrace installed. Launch it from your applications menu, or run:"
echo "  $SCRIPT_DIR/run_phylotrace.sh"

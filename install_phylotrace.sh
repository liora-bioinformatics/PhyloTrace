#!/bin/bash
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
CONDA_PATH=$( conda info --base )/bin/conda

eval "$($CONDA_PATH shell.bash hook)"

# Check if conda is installed
if ! command -v conda >/dev/null 2>&1; then
    echo -e "\e[31mError: Conda was not found. Please install or properly initialize Anaconda before running this script.\e[0m"
    exit 1
fi

echo "Conda detected. Continuing script..."

if conda env list | grep -q "PhyloTrace"; then
  echo "Environment PhyloTrace already exists. Updating the environment..."
  conda env update -f PhyloTrace.yml
else
  echo "Environment PhyloTrace does not exist. Creating the environment..."
  conda env create -f PhyloTrace.yml
fi

conda activate PhyloTrace

# Generate PhyloTrace Desktop Entry
cat > PhyloTrace.desktop << EOF
[Desktop Entry]
Name=PhyloTrace
Exec=$SCRIPT_DIR/run_phylotrace.sh
Icon=PhyloTrace
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
eval "$($CONDA_PATH shell.bash hook)"
conda activate PhyloTrace
if [ "$R_BROWSER" != "" ]; then
   Rscript -e "shiny::runApp('${SCRIPT_DIR}/App.R', launch.browser=TRUE)" > $HOME/.local/share/phylotrace/logs/last_session.log 2>&1
elif [[ $(uname -r) == *"microsoft"* ]]; then
   R_BROWSER=wslview Rscript -e "shiny::runApp('${SCRIPT_DIR}/App.R', launch.browser=TRUE)" > $HOME/.local/share/phylotrace/logs/last_session.log 2>&1
else
   R_BROWSER=xdg-open Rscript -e "shiny::runApp('${SCRIPT_DIR}/App.R', launch.browser=TRUE)" > $HOME/.local/share/phylotrace/logs/last_session.log 2>&1
fi
EOF

# Setting up the Desktop Icon
mkdir -p $HOME/.local/share/applications
mkdir -p $HOME/.local/share/icons/hicolor/scalable/apps
mkdir -p $HOME/.local/share/phylotrace/logs
mv $SCRIPT_DIR/PhyloTrace.desktop $HOME/.local/share/applications
cp $SCRIPT_DIR/www/phylo.png $HOME/.local/share/icons/hicolor/scalable/apps/PhyloTrace.png
chmod 700 $SCRIPT_DIR/run_phylotrace.sh

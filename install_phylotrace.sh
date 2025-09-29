#!/bin/bash
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
CONDA_PATH=$( conda info --base )/bin/conda

eval "$($CONDA_PATH shell.bash hook)"

# Clean cache
conda clean --all -y

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
mkdir -p $HOME/.local/share/icons/hicolor/{48x48,64x64,256x256,512x512}/apps
mkdir -p $HOME/.local/share/phylotrace/logs
mv $SCRIPT_DIR/PhyloTrace.desktop $HOME/.local/share/applications
# Copy icons to appropriate sizes
cp $SCRIPT_DIR/www/PhyloTrace_flat_48.png $HOME/.local/share/icons/hicolor/48x48/apps/PhyloTrace_flat.png
cp $SCRIPT_DIR/www/PhyloTrace_flat_64.png $HOME/.local/share/icons/hicolor/64x64/apps/PhyloTrace_flat.png
cp $SCRIPT_DIR/www/PhyloTrace_flat_256.png $HOME/.local/share/icons/hicolor/256x256/apps/PhyloTrace_flat.png
cp $SCRIPT_DIR/www/PhyloTrace_flat_512.png $HOME/.local/share/icons/hicolor/512x512/apps/PhyloTrace_flat.png
# Use 512x512 for scalable/apps
cp $SCRIPT_DIR/www/PhyloTrace_flat_512.png $HOME/.local/share/icons/hicolor/scalable/apps/PhyloTrace_flat.png
# Update index.theme
cat > $HOME/.local/share/icons/hicolor/index.theme << EOF
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
gtk-update-icon-cache -f $HOME/.local/share/icons/hicolor/ 2>/dev/null || true
chmod 700 $SCRIPT_DIR/run_phylotrace.sh
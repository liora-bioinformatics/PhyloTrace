import os
import json
import subprocess
import importlib.metadata as im
from sys import exit

# List of packages to check for
required_packages = ['pandas', 'bs4', 'requests', 'tabulate']

def install_package(package):
    """
    Installs a specified Python package using pip.
    """
    print(f"Installing {package}...")
    try:
        subprocess.check_call(['pip', 'install', package])
        print(f"Successfully installed {package}.")
    except subprocess.CalledProcessError as e:
        print(f"Error installing {package}: {e}")
        exit(1)

def check_and_install_packages(packages):
    """
    Checks if a list of packages is installed and installs them if not.
    """
    for package in packages:
        try:
            im.version(package)
        except im.PackageNotFoundError:
            print(f"Package '{package}' not found. Installing...")
            install_package(package)
        except Exception as e:
            print(f"An error occurred while checking for {package}: {e}")
            
# Run the check
check_and_install_packages(required_packages)

# Import the packages
import pandas as pd
import requests
from bs4 import BeautifulSoup
import tabulate

print("Starting script...")

fetch_online = True  # Set to False to disable online fetching

def get_conda_exe():
    """Get full path to conda executable from environment."""
    conda_exe = os.environ.get('CONDA_EXE')
    if conda_exe:
        return conda_exe
    print("CONDA_EXE not set. Falling back to 'conda' (may fail if PATH issue).")
    return 'conda'

def run_conda_command(args, quiet=True):
    """Run conda command via subprocess with optional quiet mode."""
    conda_exe = get_conda_exe()
    cmd = [conda_exe] + args
    if not quiet:
        print(f"Running command: {' '.join(cmd)}")
    try:
        result = subprocess.run(cmd, capture_output=True, text=True, check=True)
        if not quiet:
            print("Command succeeded.")
        return result.stdout, None
    except subprocess.CalledProcessError as e:
        if not quiet:
            print(f"Subprocess error (code {e.returncode}): {e.stderr}")
        return None, e.stderr
    except FileNotFoundError:
        print("Conda executable not found. Ensure env is activated.")
        return None, "Conda not found"
    except Exception as e:
        if not quiet:
            print(f"Unexpected error: {e}")
        return None, str(e)

def get_installed_packages():
    stdout, err = run_conda_command(['list', '--json'])
    if err:
        print(f"Error getting packages: {err}")
        return []
    try:
        packages = json.loads(stdout)
        print(f"Loaded {len(packages)} entries.")
        return packages
    except json.JSONDecodeError as e:
        print(f"JSON decode error: {e}")
        return []

def get_metadata_dir():
    """Get path to conda-meta directory."""
    prefix = os.environ.get('CONDA_PREFIX')
    if not prefix:
        print("No active Conda environment found (CONDA_PREFIX not set).")
        return None
    meta_dir = os.path.join(prefix, 'conda-meta')
    if not os.path.exists(meta_dir):
        print(f"Metadata dir not found: {meta_dir}")
    return meta_dir

def fetch_online_metadata(package_name, channel):
    """Fetch metadata from Anaconda.org page if local is incomplete."""
    if not fetch_online:
        return 'N/A', 'N/A', 'N/A'
    url = f"https://anaconda.org/{channel}/{package_name}"
    try:
        response = requests.get(url)
        response.raise_for_status()
        soup = BeautifulSoup(response.text, 'html.parser')
        # Extract Home URL (usually in <span class="package-summary__key">Home:</span> <a href=...>)
        home_span = soup.find('span', string='Home:')
        home_url = home_span.find_next('a').get('href', 'N/A') if home_span else 'N/A'
        # Extract License (similarly)
        license_span = soup.find('span', string='License:')
        license_ = license_span.find_next('span').text.strip() if license_span else 'N/A'
        # Author/Maintainer (maintainer often listed as user avatars or text)
        maintainer_div = soup.find('div', class_='maintainers')
        author = ', '.join([a.text.strip() for a in maintainer_div.find_all('a')]) if maintainer_div else 'N/A'
        return license_, author, home_url
    except Exception as e:
        print(f"Error fetching online for {package_name}: {e}")
        return 'N/A', 'N/A', 'N/A'

def get_package_metadata(package_name, version, build_string, channel):
    """Get license, author, and URL from local then online fallback."""
    license_ = 'N/A'
    url = 'N/A'
    author = 'N/A'
    # Local Conda metadata
    meta_dir = get_metadata_dir()
    if meta_dir:
        json_file = os.path.join(meta_dir, f"{package_name}-{version}-{build_string}.json")
        if os.path.exists(json_file):
            with open(json_file, 'r') as f:
                data = json.load(f)
                license_ = data.get('license', 'N/A')
                url = data.get('home', data.get('dev_url', data.get('doc_url', 'N/A')))
                author = data.get('author', 'N/A')
    # Local pip fallback
    if channel in ['pypi', '<pip>', 'unknown']:
        try:
            md = im.metadata(package_name)
            if license_ == 'N/A':
                license_ = md.get('License', 'N/A')
            if url == 'N/A':
                url = md.get('Home-page', 'N/A')
            if author == 'N/A':
                author = md.get('Author', 'N/A')
        except im.PackageNotFoundError:
            pass
    # Online fallback if still N/A (for Conda packages only)
    if fetch_online and channel not in ['pypi', '<pip>', 'unknown'] and (url == 'N/A' or author == 'N/A' or license_ == 'N/A'):
        online_license, online_author, online_url = fetch_online_metadata(package_name, channel)
        if license_ == 'N/A':
            license_ = online_license
        if author == 'N/A':
            author = online_author
        if url == 'N/A':
            url = online_url
    # Truncate long licenses
    if len(license_) > 100:
        license_ = license_[:100].rstrip() + "..."
    return license_, author, url

def main():
    prefix = os.environ.get('CONDA_PREFIX')
    print(f"Active env prefix: {prefix}")
    
    packages = get_installed_packages()
    total_packages = len(packages)
    print(f"Found {total_packages} entries from conda list.")
    
    data = []
    for i, pkg in enumerate(packages, 1):
        if 'name' in pkg and 'version' in pkg and 'build_string' in pkg:
            name = pkg['name']
            version = pkg['version']
            build_string = pkg['build_string']
            channel = pkg.get('channel', 'unknown').lower()  # Normalize
            print(f"Processing package {i}/{total_packages}: {name}")
            license_, author, url = get_package_metadata(name, version, build_string, channel)
            data.append({
                'Package': name,
                'Version': version,
                'License': license_,
                'Author': author,
                'URL': url
            })
    
    if data:
        df = pd.DataFrame(data)
        print(df.to_markdown(index=False))  # Preview as Markdown
        csv_file = 'conda_packages.csv'
        df.to_csv(csv_file, index=False)
        print(f"\nExported to CSV: {os.path.abspath(csv_file)}")
    else:
        if packages:
            print("Sample keys from first entry:", list(packages[0].keys()))
        print("No packages found or all entries skipped. Check if your env has Conda-installed packages (run 'conda list --json' manually and inspect keys).")

if __name__ == '__main__':
    main()
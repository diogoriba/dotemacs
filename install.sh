set +v
# Install script for linux
id -u | grep -e "^0$">/dev/null || { echo "You need super user rights to run this script"; exit 1; }
command -v rsync>/dev/null || { echo "You need to install rsync to run this script"; exit 1; }
echo Initializing git submodules
git submodule init
git submodule update
echo Creating rsync ignore file
echo install.bat > rsync.ignore
echo install.sh >> rsync.ignore
echo README.md >> rsync.ignore
echo .git >> rsync.ignore
echo rsync.ignore >> rsync.ignore
echo Installing emacs configuration files
rm -rf putty
rm -rf win32
rm diogoriba.config.elc
rm .emacs.elc
rm -rf .elisp/*.elc
sudo rsync -prv --chmod=ugo+rwx,Dugo+rwx --exclude-from rsync.ignore . ~/ || { echo "Error copying files"; }
echo Cleaning rsync ignore file
rm rsync.ignore
exit 0

function _can-install() {
    os linux && linux-variant main && can-sudo
}

function _install() {
    # Copied from the calibre website
    sudo -v &&
    wget -nv -O- https://raw.githubusercontent.com/kovidgoyal/calibre/master/setup/linux-installer.py | sudo python -c "import sys; main=lambda:sys.stderr.write('Download failed\n'); exec(sys.stdin.read()); main()"
}

function _update() {
    _install
}

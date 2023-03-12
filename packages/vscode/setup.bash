function _can-install() {
    linux-variant main &&
    graphical &&
    can-sudo &&
    package-installed python &&
    package-installed dev-tools &&
    package-installed web-dev-tools &&
    package-installed text-tools &&
    package-installed go &&
    package-installed bazel
}

function _install() {
    local TMPFILE
    sudo apt-get -y install apt-transport-https &&
    TMPFILE="$(mktemp)" &&
    wget -qO- https://packages.microsoft.com/keys/microsoft.asc | gpg --dearmor > "${TMPFILE}" &&
    sudo install -o root -g root -m 644 "${TMPFILE}" /etc/apt/trusted.gpg.d/packages.microsoft.gpg &&
    sudo sh -c 'echo "deb [arch=amd64,arm64,armhf signed-by=/etc/apt/trusted.gpg.d/packages.microsoft.gpg] https://packages.microsoft.com/repos/code stable main" > /etc/apt/sources.list.d/vscode.list' &&
    rm "${TMPFILE}" &&
    sudo apt-get update &&
    sudo apt-get -y install code &&
    code --install-extension=BazelBuild.vscode-bazel \
         --install-extension=coolbear.systemd-unit-file \
         --install-extension=DavidAnson.vscode-markdownlint \
         --install-extension=eamodio.gitlens \
         --install-extension=foxundermoon.shell-format \
         --install-extension=jetmartin.bats \
         --install-extension=mads-hartmann.bash-ide-vscode \
         --install-extension=ms-python.python \
         --install-extension=redhat.ansible \
         --install-extension=redhat.vscode-yaml \
         --install-extension=streetsidesoftware.code-spell-checker \
         --install-extension=timonwong.shellcheck \
         --install-extension=VisualStudioExptTeam.vscodeintellicode \
         --install-extension=wholroyd.jinja
}

function _update() {
    _install
}

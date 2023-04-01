function _install() {
    dotfiles-can-sudo && package-installed dev-tools
}

function _install() {
    sudo apt-get install -y autoconf libssl-dev libyaml-dev libreadline6-dev zlib1g-dev libgmp-dev libncurses5-dev libffi-dev libgdbm6 libgdbm-dev libdb-dev uuid-dev &&
    source "${PACKAGE_CONF_DIR}/env.sh" || return 1

    if [ ! -d "${DOTFILES_RBENV_ROOT}" ]; then
        git clone https://github.com/rbenv/rbenv.git "${DOTFILES_RBENV_ROOT}" || return 1
    fi

    cd "${DOTFILES_RBENV_ROOT}" &&
    git checkout v1.2.0 &&
    src/configure &&
    make -C src || return 1

    if [ ! -d "${DOTFILES_RBENV_ROOT}/plugins/ruby-build" ]; then
        git clone https://github.com/rbenv/ruby-build.git "${DOTFILES_RBENV_ROOT}/plugins/ruby-build" || return 1
    fi

    cd "${DOTFILES_RBENV_ROOT}/plugins/ruby-build" &&
    git checkout v20220910.1 &&
    zsh -c "source \"${PACKAGE_CONF_DIR}/env.sh\" && source \"${PACKAGE_CONF_DIR}/aliases.sh\"" &&
    export PATH="${DOTFILES_RBENV_ROOT}/shims:${PATH}" &&
    gem update --system &&
    gem install bundler &&
    gem update bundler
}

function _update() {
    _install
}

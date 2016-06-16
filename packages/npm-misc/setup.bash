function _can-install() {
    package-installed nodejs &&
    type npm &>/dev/null &&
    [ ! -z "${NPM_PACKAGES}" ]
}

function _install() {
    if [ -d "${HOME}/tmp" ]; then
        rmdir "${HOME}/tmp" || return 1
    fi

    npm install -g csslint eslint jscs jsonlint js-yaml
}

function _update() {
    _install
}

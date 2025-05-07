source "${PACKAGE_INSTALL_DIR}/zsh-nvm.plugin.zsh"

if ! type npm &>/dev/null; then
    nvm install node
    nvm install-latest-npm
fi

DOTFILES_NVM_DEFAULT="$(nvm current)"

function dotfiles-nvm-version-check() {
    local NVMRC

    local DIR
    DIR="$(readlink -f .)"
    while true; do
        if [ -f "${DIR}/.nvmrc" ]; then
            NVMRC="${DIR}/.nvmrc"
            break
        elif [ "${DIR}" = '/' ]; then
            break
        else
            DIR="$(readlink -f "${DIR}/..")"
        fi
    done

    local VERSION
    if [ -n "${NVMRC}" ]; then
        VERSION="$(cat "${NVMRC}")"
        DOTFILES_NVM_VERSION="${VERSION}"
    else
        VERSION="${DOTFILES_NVM_DEFAULT}"
        unset DOTFILES_NVM_VERSION
    fi

    if [ "${VERSION}" != "$(nvm current)" ]; then
        nvm use "${VERSION}" >/dev/null
    fi
}

dotfiles-nvm-version-check

precmd_functions=(dotfiles-nvm-version-check $precmd_functions)

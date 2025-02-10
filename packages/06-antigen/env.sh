# shellcheck shell=sh

# Options for zsh-nvm
export NVM_DIR="${HOME}/.nvm"
export NVM_LAZY_LOAD=true
export NVM_COMPLETION=true

if [ -f "${NVM_DIR}/nvm.sh" ] && . "${NVM_DIR}/nvm.sh"; then
    nvm use node
    DOTFILES_NVM_LIB_PATH="$(readlink -f "$(dirname "$(nvm which current)")/../lib/node_modules")"
    export DOTFILES_NVM_LIB_PATH
fi

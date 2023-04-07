# shellcheck shell=sh

# Options for zsh-nvm
export NVM_DIR="${HOME}/.nvm"
export NVM_LAZY_LOAD=true
export NVM_COMPLETION=true

if [ -f "${NVM_DIR}/nvm.sh" ]; then
    . "${NVM_DIR}/nvm.sh" && nvm use node
fi

DOTFILES_NVM_LIB_PATH="$(zsh -c "PACKAGE_INSTALL_DIR=\"${PACKAGE_INSTALL_DIR}\" && source \"${PACKAGE_SOURCE_DIR}/aliases.zsh\" && readlink -f \"\$(dirname \"$(nvm which current)\")/../lib/node_modules)\"")"
export DOTFILES_NVM_LIB_PATH

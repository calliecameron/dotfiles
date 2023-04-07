# shellcheck shell=sh

# Options for zsh-nvm
export NVM_DIR="${HOME}/.nvm"
export NVM_LAZY_LOAD=true
export NVM_COMPLETION=true

if [ -f "${NVM_DIR}/nvm.sh" ]; then
    . "${NVM_DIR}/nvm.sh" && nvm use node
fi

MY_LESS="${LESS}"

source "${PACKAGE_INSTALL_DIR}/antigen.zsh"
antigen use oh-my-zsh
antigen bundle command-not-found
antigen bundle zsh-users/zsh-syntax-highlighting

if [ -z "${DOTFILES_ANTIGEN_CORE_ONLY}" ]; then
    antigen bundle callumcameron/argtypes
    antigen bundle callumcameron/argus
    antigen bundle callumcameron/distributor
    antigen bundle callumcameron/markdown-makefile
fi

antigen bundle callumcameron/emacs-launchers
antigen apply

# Don't mess with my LESS, oh-my-zsh!
export LESS="${MY_LESS}"
unset MY_LESS

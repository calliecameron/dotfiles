MY_LESS="${LESS}"

source "${PACKAGE_INSTALL_DIR}/antigen.zsh"
antigen use oh-my-zsh
antigen bundle command-not-found
antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle vagrant
antigen bundle zsh-users/zsh-completions
antigen apply

# Don't mess with my LESS, oh-my-zsh!
export LESS="${MY_LESS}"
unset MY_LESS

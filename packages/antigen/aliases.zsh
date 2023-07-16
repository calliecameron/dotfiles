MY_LESS="${LESS}"

source "${PACKAGE_INSTALL_DIR}/antigen/antigen.zsh"
antigen use oh-my-zsh
antigen bundle command-not-found
antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle vagrant
antigen bundle zsh-users/zsh-completions
antigen bundle lukechilds/zsh-nvm
antigen bundle lukechilds/zsh-better-npm-completion
antigen bundle fzf
antigen bundle fd
antigen apply

# Don't mess with my LESS, oh-my-zsh!
export LESS="${MY_LESS}"
unset MY_LESS

if ! type npm &>/dev/null; then
    nvm install node
fi

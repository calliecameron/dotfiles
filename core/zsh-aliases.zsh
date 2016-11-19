# -*- Shell-script -*-
#
# Custom aliases and other things that should only be visible to an
# interactive shell go in here.
#

test ! -z "${DOTFILES_PROFILING}" && printf 'zsh ' && date --rfc-3339=ns

export DOTFILES_SHELL='zsh'
# Might not have loaded environment variables yet, so have to do this with the full path
source "${DOTFILES_DIR}/core/generic-aliases.bash"

if [ -z "${DOTFILES_ZSH_NO_DEFAULT_PROMPT}" ]; then
PROMPT="%n@%m:%~
\$ "
fi

# Stop cd suggesting usernames
zstyle ':completion:*' users

autoload -U compinit
compinit

test ! -z "${DOTFILES_PROFILING}" && printf 'zsh ' && date --rfc-3339=ns; true

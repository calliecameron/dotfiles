# Make less more friendly for non-text input files, if lesspipe exists
test -x /usr/bin/lesspipe && eval "$(SHELL=/bin/sh lesspipe)"

# Enable colour support in ls and grep
unalias 'll' 'la' 'ls' &>/dev/null
if [ -x /usr/bin/dircolors ]; then
    function ll() { command ls -lhFv --color=auto "${@}"; }
    function la() { command ls -AlhFv --color=auto "${@}"; }
    function ls() { command ls -CFv --color=auto "${@}"; }

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
    alias rgrep='rgrep --color=auto'
else
    function ll() { command ls -lhFv "${@}"; }
    function la() { command ls -AlhFv "${@}"; }
    function ls() { command ls -CFv "${@}"; }
fi

alias gr='grep'
alias rg='rgrep'
alias df='df -h'
alias du='du -h'
alias free='free -h'
alias lc='wc -l'
alias untar='tar -xvf'
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'
alias ......='cd ../../../../..'
alias .......='cd ../../../../../..'
alias ........='cd ../../../../../../..'
alias t='tree -C'
alias m='make'
alias x='dtrx'

if [ "${DOTFILES_OS}" = 'linux' ]; then
    alias cbc='xclip -i -selection clipboard'
    alias cbp='xclip -o -selection clipboard'

    if [ "${DOTFILES_LINUX_VARIANT}" = 'main' ]; then
        alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'
    fi
fi

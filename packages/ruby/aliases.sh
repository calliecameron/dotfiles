# shellcheck shell=bash

eval "$(rbenv init -)"

if ! rbenv versions | grep "${DOTFILES_RBENV_RUBY_VERSION}" >/dev/null 2>/dev/null; then
    rbenv install "${DOTFILES_RBENV_RUBY_VERSION}"
    rbenv rehash
fi

rbenv global "${DOTFILES_RBENV_RUBY_VERSION}"

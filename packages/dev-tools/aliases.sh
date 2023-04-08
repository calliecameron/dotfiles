# shellcheck shell=bash

# Autoenv overwites cd, so we have to load it later to avoid it messing with
# package loading.
DOTFILES_AUTOENV_SOURCE="$(npm root -g)/@hyperupcall/autoenv/activate.sh"
export DOTFILES_AUTOENV_SOURCE

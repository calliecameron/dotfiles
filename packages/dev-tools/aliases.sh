# shellcheck shell=bash

# Autoenv overwites cd, so we have to load it later to avoid it messing with
# package loading.
export DOTFILES_AUTOENV_SOURCE="${PACKAGE_INSTALL_DIR}/autoenv/activate.sh"

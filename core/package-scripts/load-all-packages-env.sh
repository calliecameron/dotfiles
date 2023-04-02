# shellcheck shell=dash
# shellcheck disable=SC2317
# Must work in dash, bash and zsh

. "${DOTFILES_PACKAGE_SCRIPTS}/load-package-env.sh"
. "${DOTFILES_PACKAGE_SCRIPTS}/common-funcs.sh"

packageloop 'Environment' loadpackageenv

commonfuncscleanup
loadpackageenvcleanup

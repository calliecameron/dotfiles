# shellcheck shell=sh

export PATH="${PACKAGE_INSTALL_DIR}/bin:${PATH}"

dotfiles-home-bin-link ccache gcc g++ cc c++
dotfiles-home-bin-link ctags-exuberant ctags etags

export BATS_LIB_PATH="${DOTFILES_NVM_LIB_PATH}"

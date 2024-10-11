# shellcheck shell=sh

export DOTFILES_NIXPKGS_VERSION='24.05'

export DOTFILES_NIXPKGS="nixpkgs/release-${DOTFILES_NIXPKGS_VERSION}"
export DOTFILES_NIX_PROFILE_DIR="${PACKAGE_INSTALL_DIR}/profiles"
export DOTFILES_NIX_PROFILE="${DOTFILES_NIX_PROFILE_DIR}/dotfiles-${DOTFILES_NIXPKGS_VERSION}"
export LOCALE_ARCHIVE='/usr/lib/locale/locale-archive'

dotfiles-home-link "${PACKAGE_SOURCE_DIR}/nix.conf" "${HOME}/.config/nix/nix.conf"

if [ -e '/etc/profile.d/nix.sh' ]; then
    . '/etc/profile.d/nix.sh'
fi

# This stops interactive shells trying to load nix again and messing up the path order
export __ETC_PROFILE_NIX_SOURCED

if command -v nix-env >/dev/null 2>/dev/null; then
    nix-env --switch-profile "${DOTFILES_NIX_PROFILE}"
fi

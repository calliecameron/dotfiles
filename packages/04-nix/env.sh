# shellcheck shell=sh

export XDG_DATA_DIRS="${HOME}/.nix-profile/share:${XDG_DATA_DIRS}"

export DOTFILES_NIXPKGS_VERSION='25.11'

export DOTFILES_NIXPKGS_CHANNEL="nixos-${DOTFILES_NIXPKGS_VERSION}"
export DOTFILES_NIXPKGS_CHANNEL_UNSTABLE='nixos-unstable'
export DOTFILES_NIX_PROFILE_DIR="${HOME}/.local/state/nix/profiles"
export DOTFILES_NIX_PROFILE="${DOTFILES_NIX_PROFILE_DIR}/dotfiles-${DOTFILES_NIXPKGS_VERSION}"
export DOTFILES_NIX_CHANNELS="${DOTFILES_NIX_PROFILE_DIR}/channels"
export DOTFILES_NIX_GC_AFTER='60d'
export LOCALE_ARCHIVE='/usr/lib/locale/locale-archive'

export DOTFILES_HOME_MANAGER_DIR="${DOTFILES_DIR}/home-manager"
export DOTFILES_HOME_MANAGER_FAKE_HOME="${DOTFILES_LOCAL_DIR}/home-manager"
export DOTFILES_HOME_MANAGER_PROFILE="${DOTFILES_HOME_MANAGER_FAKE_HOME}/profile"
export DOTFILES_HOME_MANAGER_BASHRC="${DOTFILES_HOME_MANAGER_FAKE_HOME}/bashrc"
export DOTFILES_HOME_MANAGER_ZSHENV="${DOTFILES_HOME_MANAGER_FAKE_HOME}/.zshenv"
export DOTFILES_HOME_MANAGER_ZSHRC="${DOTFILES_HOME_MANAGER_FAKE_HOME}/.zshrc"

export DOTFILES_LOCAL_HOME_MANAGER="${HOME}/.dotfiles-home-manager.nix"

dotfiles-home-link "${PACKAGE_SOURCE_DIR}/nix.conf" "${HOME}/.config/nix/nix.conf"

if [ -e '/etc/profile.d/nix.sh' ]; then
    . '/etc/profile.d/nix.sh'
fi

# This stops interactive shells trying to load nix again and messing up the path order
export __ETC_PROFILE_NIX_SOURCED

if command -v nix-env >/dev/null 2>/dev/null; then
    nix-env --switch-profile "${DOTFILES_NIX_PROFILE}"
fi

if [ -e "${DOTFILES_HOME_MANAGER_PROFILE}" ]; then
    . "${DOTFILES_HOME_MANAGER_PROFILE}"
fi

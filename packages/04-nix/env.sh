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

export DOTFILES_HOME_MANAGER="${DOTFILES_DIR}/home-manager"
export DOTFILES_LOCAL_HOME_MANAGER="${HOME}/.dotfiles-home-manager.nix"
export DOTFILES_HOME_MANAGER_SESSION_VARS="${DOTFILES_NIX_PROFILE}/etc/profile.d/hm-session-vars.sh"

dotfiles-home-link "${PACKAGE_SOURCE_DIR}/nix.conf" "${HOME}/.config/nix/nix.conf"

if [ -e '/etc/profile.d/nix.sh' ]; then
    . '/etc/profile.d/nix.sh'
fi

# This stops interactive shells trying to load nix again and messing up the path order
export __ETC_PROFILE_NIX_SOURCED

if command -v nix-env >/dev/null 2>/dev/null; then
    nix-env --switch-profile "${DOTFILES_NIX_PROFILE}"
fi

if [ -e "${DOTFILES_HOME_MANAGER_SESSION_VARS}" ]; then
    . "${DOTFILES_HOME_MANAGER_SESSION_VARS}"
fi

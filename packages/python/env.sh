# shellcheck shell=sh

if command -v pygmentize >/dev/null; then
    dotfiles-home-link "${PACKAGE_SOURCE_DIR}/lessfilter"
fi

export WORKON_HOME="${HOME}/.virtualenvs"
mkdir -p "${WORKON_HOME}"
VIRTUALENVWRAPPER_PYTHON="$(readlink -f "$(command -v python3)")"
export VIRTUALENVWRAPPER_PYTHON
dotfiles-home-link "${PACKAGE_SOURCE_DIR}/postmkvirtualenv" "${WORKON_HOME}/postmkvirtualenv"

export PYENV_ROOT="${HOME}/.pyenv"
export PATH="${PYENV_ROOT}/bin:${PATH}"
if command -v pyenv >/dev/null; then
    eval "$(pyenv init --path)"
fi

export DOTFILES_PIPX_INSTALL_DIR="${HOME}/.pipx-install"
export PIPX_BIN_DIR="${DOTFILES_PIPX_INSTALL_DIR}/bin"
export PIPX_MAN_DIR="${DOTFILES_PIPX_INSTALL_DIR}/share/man"
export PATH="${PIPX_BIN_DIR}:${PATH}"

export DOTFILES_UV_INSTALL_DIR="${HOME}/.uv-install"
export UV_TOOL_BIN_DIR="${DOTFILES_UV_INSTALL_DIR}/bin"
export PATH="${UV_TOOL_BIN_DIR}:${PATH}"
dotfiles-home-link "${PACKAGE_SOURCE_DIR}/uv-global.toml" "${HOME}/.config/uv/uv.toml"
export DOTFILES_UV_ZSH_COMPLETIONS_DIR="${PACKAGE_INSTALL_DIR}/uv-zsh-completions"

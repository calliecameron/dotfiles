# shellcheck shell=sh

if command -v pygmentize >/dev/null; then
    dotfiles-home-link "${PACKAGE_SOURCE_DIR}/lessfilter"
fi

export WORKON_HOME="${HOME}/.virtualenvs"
mkdir -p "${WORKON_HOME}"
VIRTUALENVWRAPPER_PYTHON="$(readlink -f "$(command -v python3)")"
export VIRTUALENVWRAPPER_PYTHON
dotfiles-home-link "${PACKAGE_SOURCE_DIR}/postmkvirtualenv" "${WORKON_HOME}/postmkvirtualenv"

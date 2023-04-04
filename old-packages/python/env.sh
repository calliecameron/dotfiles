if which pygmentize >/dev/null; then
    dotfiles-home-link "${PACKAGE_SOURCE_DIR}/lessfilter"
fi

# This is where pip --user installs binaries
export PATH="${HOME}/.local/bin:${PATH}"

export WORKON_HOME="${HOME}/.virtualenvs"
mkdir -p "${WORKON_HOME}"
# shellcheck disable=SC2155
export VIRTUALENVWRAPPER_PYTHON="$(readlink -f "$(which python3)")"
dotfiles-home-link "${PACKAGE_SOURCE_DIR}/postmkvirtualenv" "${WORKON_HOME}/postmkvirtualenv"

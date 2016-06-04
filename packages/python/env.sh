if which pygmentize >/dev/null; then
    homelink "${PACKAGE_CONF_DIR}/lessfilter"
fi

export WORKON_HOME="${HOME}/.virtualenvs"
mkdir -p "${WORKON_HOME}"
# shellcheck disable=SC2155
export VIRTUALENVWRAPPER_PYTHON="$(readlink -f "$(which python3)")"
homelink "${PACKAGE_CONF_DIR}/postmkvirtualenv" "${WORKON_HOME}/postmkvirtualenv"

homelink "${PACKAGE_CONF_DIR}/ipython_config.py" "${HOME}/.ipython/profile_default/ipython_config.py"

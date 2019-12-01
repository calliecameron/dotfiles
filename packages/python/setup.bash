function _install() {
    os linux && can-sudo
}

function _install() {
    sudo apt-get -y install python-dev python-pip python3-dev python3-pip &&
    # Make sure we're using the local pip if it exists, not the apt-installed one
    LOCAL_PIP_DIR="${HOME}/.local/bin" &&
    export PATH="${LOCAL_PIP_DIR}:${PATH}" &&
    "${LOCAL_PIP_DIR}/pip" install --user --upgrade pip setuptools &&
    "${LOCAL_PIP_DIR}/pip3" install --user --upgrade pip setuptools &&
    "${LOCAL_PIP_DIR}/pip" install --user --upgrade future gitpython ipython pyinotify pytimeparse rlipython &&
    "${LOCAL_PIP_DIR}/pip3" install --user --upgrade autoenv autopep8 future gitpython flake8 importmagic 'ipython<7' jupyter 'jupyter-console<6' jedi mypy pandoc-include pygments pyinotify pylint pytimeparse pep8 rlipython rope snakeviz virtualenv virtualenvwrapper yapf
}

function _update() {
    _install
}

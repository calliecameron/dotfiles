function _install() {
    os linux && can-sudo
}

function _install() {
    sudo apt-get -y install python3-dev python3-pip &&
    # Make sure we're using the local pip if it exists, not the apt-installed one
    LOCAL_PIP_DIR="${HOME}/.local/bin" &&
    export PATH="${LOCAL_PIP_DIR}:${PATH}" &&
    if [ -f "${LOCAL_PIP_DIR}/pip3" ]; then
        BASE_PIP="${LOCAL_PIP_DIR}/pip3"
    else
        BASE_PIP='pip3'
    fi
    "${BASE_PIP}" install --user --upgrade pip setuptools wheel &&
    "${LOCAL_PIP_DIR}/pip3" install --user --upgrade autoenv autopep8 future gitpython flake8 importmagic ipython jupyter jupyter-console jedi mypy pandoc-include pdfminer pygments pyinotify pylint pytimeparse pep8 rlipython rope snakeviz virtualenv virtualenvwrapper yapf
}

function _update() {
    _install
}

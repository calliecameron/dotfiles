function _install() {
    os linux && can-sudo
}

function _install() {
    sudo apt-get -y install python-dev python-pip python3-dev python3-pip &&
    # Make sure we're using the local pip if it exists, not the apt-installed one
    export PATH="${HOME}/.local/bin:${PATH}" &&
    pip install --user --upgrade pip setuptools &&
    pip3 install --user --upgrade pip setuptools &&
    pip install --user --upgrade future gitpython ipython pyinotify pytimeparse rlipython &&
    pip3 install --user --upgrade autoenv autopep8 future gitpython flake8 importmagic ipython jedi mypy pygments pyinotify pylint pytimeparse pep8 rlipython rope virtualenv virtualenvwrapper yapf
}

function _update() {
    _install
}

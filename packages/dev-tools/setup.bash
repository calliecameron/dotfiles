function _can-install() {
    # Depends on antigen for npm
    os linux && linux-variant main && can-sudo && package-installed antigen
}

function _install() {
    sudo apt-get -y install bash-doc bison bison-doc build-essential ccache clang clang-format cmake cppcheck devscripts exuberant-ctags flex global info make-doc man-db manpages-posix manpages-posix-dev shellcheck valgrind &&
    npm install -g bats bats-support bats-assert
}

function _update() {
    _install
}

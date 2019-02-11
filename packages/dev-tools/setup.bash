function _can-install() {
    os linux && linux-variant main pi && can-sudo
}

function _install() {
    sudo apt-get -y install bash-doc bison bison-doc build-essential ccache clang clang-format cmake cppcheck devscripts exuberant-ctags flex global info make-doc man-db manpages-posix manpages-posix-dev shellcheck valgrind
}

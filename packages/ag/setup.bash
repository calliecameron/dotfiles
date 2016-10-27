function _can-install() {
    (os linux && known-linux-variant && can-sudo) || os cygwin
}

function _install() {
    if os linux; then
        sudo apt-get -y install pkg-config libpcre3-dev liblzma-dev || return 1
    elif os cygwin; then
        apt-cyg install pkg-config libpcre-devel liblzma-devel || return 1
    else
        echo-red "Unknown OS"
        return 1
    fi
    # shellcheck disable=SC2155
    local TMPDIR="$(mktemp -d)" &&
    cd "${TMPDIR}" &&
    wget 'http://geoff.greer.fm/ag/releases/the_silver_searcher-0.33.0.tar.gz' &&
    tar -xf 'the_silver_searcher-0.33.0.tar.gz' &&
    cd 'the_silver_searcher-0.33.0' &&
    mkdir "${PACKAGE_INSTALL_DIR}" &&
    ./configure "--prefix=${PACKAGE_INSTALL_DIR}" &&
    make &&
    make install &&
    cd &&
    rm -r "${TMPDIR}"
}

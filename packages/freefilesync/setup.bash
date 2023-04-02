function _can-install() {
    dotfiles-linux-variant main && dotfiles-graphical
}

function _install() {
    mkdir -p "${PACKAGE_INSTALL_DIR}" &&
    cd "${PACKAGE_INSTALL_DIR}" &&
    wget https://download944.mediafire.com/68gdfz4z383g/gy66u6oa9ftddgs/FreeFileSync_10.9_Linux.tar.gz &&
    echo 'b8326b87c5349b7798b6047e49b425580eef12aaefe57b3e2c0609dd1a39d540  FreeFileSync_10.9_Linux.tar.gz' > checksum &&
    sha256sum -c checksum &&
    tar -xf FreeFileSync_10.9_Linux.tar.gz &&
    cd FreeFileSync &&
    chmod a-x CHANGELOG LICENSE RealTimeSync Resources.zip 'User Manual.pdf' cacert.pem ding.wav gong.wav harp.wav styles.gtk_rc Bin/FreeFileSync_i686 Bin/RealTimeSync_i686 Bin/RealTimeSync_x86_64 &&
    mkdir -p "${PACKAGE_INSTALL_DIR}/bin" &&
    cp "${PACKAGE_SOURCE_DIR}/freefilesync" "${PACKAGE_INSTALL_DIR}/bin"
}

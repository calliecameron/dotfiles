function _can-install() {
    os linux && linux-variant android
}

function _install() {
    "${PACKAGE_CONF_DIR}/setup-android-openbox.sh"
}

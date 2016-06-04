#!/bin/bash
# Create an Ubuntu 14.04 image for LinuxOnAndroid

THIS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ARCHIVE='ubuntu-core-14.04.4-core-armhf.tar.gz'
MAIN_IMG='ubuntu-14.04.CORE.ext2.img'

# Change this to control the size of the image; between 1 and 4
IMAGE_SIZE_GB=4


function fail() {
    echo "Something went wrong!"
    exit 1
}


sudo apt-get install qemu-user-static &&
# shellcheck disable=SC2015
LOOPDEV="$(sudo losetup -f)" || fail


function download-archive() {
    if [ ! -e "${ARCHIVE}" ]; then
        wget "http://cdimage.ubuntu.com/ubuntu-core/releases/14.04/release/${ARCHIVE}" || return 1
    fi
}

function create-image() {
    local IMG="${1}"
    local SIZE="${2}"
    if [ -e "${IMG}" ]; then
        echo "${IMG} already exists; won't overwrite it!"
        return 1
    fi
    truncate -s "${SIZE}G" "${IMG}" &&
    sudo losetup "${LOOPDEV}" "${IMG}" &&
    sudo mkfs.ext2 "${LOOPDEV}" &&
    mkdir image &&
    sudo mount "${LOOPDEV}" image &&
    sudo cp "${ARCHIVE}" "image/${ARCHIVE}" &&
    cd image &&
    sudo tar -xf "${ARCHIVE}" &&
    sudo rm "${ARCHIVE}" &&
    cd .. &&
    sudo umount image &&
    sudo losetup -d "${LOOPDEV}" &&
    rmdir image
}

function mount-image() {
    local IMG="${1}"
    sudo losetup "${LOOPDEV}" "${IMG}" &&
    mkdir image &&
    sudo mount "${LOOPDEV}" image
}

function unmount-image() {
    sudo umount image &&
    sudo losetup -d "${LOOPDEV}" &&
    rmdir image
}

function run-in-image() {
    local CMD="${1}"
    cd image &&
    sudo cp /usr/bin/qemu-arm-static usr/bin &&
    sudo cp etc/resolv.conf etc/resolv.conf.saved &&
    sudo cp /etc/resolv.conf etc/resolv.conf &&
    sudo mount --bind /sys sys &&
    sudo mount --bind /proc proc &&
    sudo mount --bind /dev dev &&
    sudo mount --bind /dev/pts dev/pts &&
    sudo LC_ALL=C chroot . "${CMD}" &&
    sudo umount dev/pts &&
    sudo umount dev &&
    sudo umount proc &&
    sudo umount sys &&
    sudo mv etc/resolv.conf.saved etc/resolv.conf &&
    sudo rm usr/bin/qemu-arm-static &&
    cd ..
}

function build-main-image() {
    create-image "${MAIN_IMG}" "${IMAGE_SIZE_GB}" &&
    mount-image "${MAIN_IMG}" &&
    sudo cp "${THIS_DIR}/scripts/universe.list" image/etc/apt/sources.list.d/universe.list &&
    sudo chmod a+r image/etc/apt/sources.list.d/universe.list &&
    sudo cp "${THIS_DIR}/scripts/init.sh" image/root/init.sh &&
    sudo cp -r "${THIS_DIR}/scripts/vnc" image/root/.vnc &&
    sudo cp "${THIS_DIR}/scripts/setup-packages.sh" image/setup-packages.sh &&
    sudo mkdir image/android &&
    sudo mkdir image/sdcard &&
    sudo mkdir image/external_sd &&
    sudo mkdir image/root/cfg &&
    run-in-image /setup-packages.sh &&
    sudo rm image/setup-packages.sh &&
    unmount-image
}


download-archive &&
# shellcheck disable=SC2015
build-main-image || fail

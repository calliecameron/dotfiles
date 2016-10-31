Linux on Android
================

Configuration instructions for a chrooted Ubuntu 14.04 image using the
LinuxOnAndroid app. This will be easiest if Android Terminal IDE is
set up first.

1. Device must be rooted.
2. Kernel must support loopback devices: `zcat /proc/config.gz | grep
   CONFIG_BLK_DEV_LOOP`.
3. Install apps: LinuxOnAndroid, Terminal Emulator, and bVNC Free.
4. On a desktop Ubuntu machine, `./create-image.sh` to prepare the
   filesystem image for use.
5. Copy the image to the Android device and install it as instructed
   in LinuxOnAndroid.
6. When starting up, start the VNC server. The correct screen size for
   the Asus Transformer Prime is 1280x750.
7. Log in using VNC: password is 'ubuntu'
8. `sudo apt-get install git`.
9. Install dotfiles as normal.
10. See ()[../../packages/04-common-dirs/README.md] for environment
    variables you can set to make things more convenient.

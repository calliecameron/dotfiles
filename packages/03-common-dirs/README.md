Common dirs
===========

When on Linux dual-booted with Windows, you can set some environment
variables in `~/.dotfiles-variables.sh` to make accessing the Windows
partition more convenient; these paths will be symlinked into home
(only works if the Windows partition is mounted).

- `WINROOT`: Windows partition's mount point
- `WINHOME`: Windows user's home directory
- `WINDOWNLOADS`: Windows user's downloads directory
- `CYGHOME`: Windows user's Cygwin home directory

A few similar variables exist for Android Linux:

- `ANDROID_SDCARD`
- `ANDROID_DOCUMENTS`
- `ANDROID_DOWNLOADS`
- `ANDROID_EXTRA`: because the Android Linux image is limited to 4 GB,
  this should point to a folder outside the image where extra stuff
  can be downloaded to (currently used by Dash docsets)

On Android Linux, you can also set `DROPBOX` to point to the path used
by e.g. Dropsync (on Mint, Dropbox goes in the home directory and is
detected automatically).

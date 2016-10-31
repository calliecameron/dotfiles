Extra stuff for Mint
====================

Trackpad settings on laptops
----------------------------

1. Preferences -> Mouse and Touchpad
2. If the trackpad is annoying, disable 'mouse clicks with touchpad'
3. Enable two finger scrolling if supported


Access to Windows partitions on dual boot
-----------------------------------------

1. Use `sudo blkid` to find the UUID of the Windows partition.
2. Add a line like the following to /etc/fstab:
    UUID=<UUID>    /mnt/windows    ntfs-3g    defaults,inherit,permissions    0    0
3. Create as root the mount point, e.g. /mnt/windows. The mount point
   should NOT be under /media, or it will be considered a removable
   drive.
4. Make sure the mount works, with `sudo mount -a`.
5. Unmount the partition with `sudo umount /mnt/windows`.
6. Create user mapping: run `sudo ntfs-3g.usermap /dev/<dev>`, where
   <dev> is the partition in question. This creates a UserMapping
   file.
7. Mount the partition again, and put the UserMapping file in a folder
   '.NTFS-3G' in the root of the NTFS partition.
8. Unmount and remount the partition so that the user mapping takes effect.
9. See [](../packages/04-common-dirs/README.md) for environment
   variables you can set to make accessing the Windows partition more
   convenient.


Samba shared folders
--------------------

Right click on folder in Nemo, 'Sharing Options'; ports to open: UDP
137 & 138, TCP 139 & 445.


Truecrypt
-----------------

Settings:
- Untick 'preserve modification timestamp of file containers'
- Tick 'do not use kernel cryptographic services'

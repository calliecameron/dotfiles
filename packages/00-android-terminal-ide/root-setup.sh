# This file sets up paths on Android.
# No shebang here since this is to set up the interpreter paths! Must be run as root.

mount -o remount,rw -t rootfs rootfs /
mkdir -p /bin
mkdir -p /usr/bin
chmod ugo+rwx /bin /usr/bin
ln -s /data/data/com.spartacusrex.spartacuside/files/system/bin/bash /bin/bash
ln -s /data/data/com.spartacusrex.spartacuside/files/system/bin/bbdir/env /usr/bin/env
ln -s /data/data/com.googlecode.pythonforandroid/files/python/bin/python /usr/bin/python

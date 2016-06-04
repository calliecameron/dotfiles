if [ "${DOTFILES_OS}" = 'android' ]; then

    # Setup /bin, etc.
    MUTEX="${HOME}/.dotfiles-android-mutex"

    while ! mkdir "${MUTEX}" >/dev/null 2>/dev/null; do
        sleep 1
    done

    if [ ! -d '/bin' ]; then
        su -c "sh '${PACKAGE_CONF_DIR}/root-setup.sh'"
    fi

    rmdir "${MUTEX}"

    unset MUTEX


    #######################################
    # From the Terminal IDE default .bashrc
    export IDESYSTEM="${HOME}/system"
    export TERMINFO="${IDESYSTEM}/etc/terminfo"
    export TEMP="${HOME}/tmp"
    export TMPDIR="${TEMP}"
    export ODEX_FOLDER="${TEMP}"
    export SHELL="${IDESYSTEM}/bin/bash"
    export MC_DATADIR="${IDESYSTEM}/etc/mc"
    export VIMRUNTIME="${IDESYSTEM}/etc/vim"

    # Some default values - this is used by telnetd
    export TELNET_PORT='8080'

    # End of Terminal IDE stuff
    #######################################

    export PATH="/bin:/usr/bin:${PATH}"

    # Set up python
    export DOTFILES_PYANDROID='/data/data/com.googlecode.pythonforandroid'
    export PYTHONHOME="${DOTFILES_PYANDROID}/files/python"
    export PYTHONPATH="${PYTHONPATH}:/mnt/sdcard/com.googlecode.pythonforandroid/extras/python/"
    export PYTHONPATH="${PYTHONPATH}:${DOTFILES_PYANDROID}/files/python/lib/python2.6/lib-dynload"
    export LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:${DOTFILES_PYANDROID}/files/python/lib"

fi

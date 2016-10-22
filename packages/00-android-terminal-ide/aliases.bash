if os android; then
    #######################################
    # From the Terminal IDE default .bashrc

    # Check if we are running over telnet or ssh
    # This variable is set in the telnetd script
    if [ "${TELNET_ON}" = yes ]; then
        # You may need to set a different TERM value
        #export TERM='xterm'

        # Need to resize the screen
        resize
    fi

    HISTFILE="${HOME}/.bash_history"
    # Reload the history - as bash original starts from wrong home dir...
    history -r

    # Change to the HOME folder
    # shellcheck disable=SC2164
    cd "${HOME}"

    # End of Terminal IDE stuff
    ###########################

    PS1='android@\h:\w\n\$ '
fi

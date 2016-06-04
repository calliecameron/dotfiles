# If you SSH into a host where 'hostname -f' doesn't give a fully qualified domain name
# (either because it isn't set in /etc/hostname (as on Ubuntu), or because it *doesn't
# have* a FQDN (as on a LAN), use the IP address instead. (Note that this isn't foolproof:
# if someone puts a dot in the hostname (is that even allowed?) it will get confused.)
if [[ "$(hostname -f)" != *.* ]] && [ ! -z "${SSH_CONNECTION}" ]; then
    export EMACS_PROMPT_HOSTNAME="$(echo "${SSH_CONNECTION}" | awk '{print $3}')"
else
    export EMACS_PROMPT_HOSTNAME="$(hostname)"
fi

function emacs-dir-tracking() {
    command emacs-dir-tracking
}

export PAGER='emacs-pager-wrapper'
alias less='emacs-pager-wrapper'

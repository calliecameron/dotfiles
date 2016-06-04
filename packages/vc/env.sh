export DOTFILES_HG_USERNAME="${HOME}/.dotfiles-hg-username"
export DOTFILES_HG_LOCAL="${HOME}/.dotfiles-hg-local"

homelink "${PACKAGE_CONF_DIR}/hgrc"

# Platform-specific settings
if [ "${DOTFILES_OS}" = 'android' ]; then
    # Make sure git actually works
    export GIT_SSH="${PACKAGE_CONF_DIR}/android-ssh-with-key"

    if [ ! -z "${DOTFILES_VC_NAME}" ] && [ ! -z "${DOTFILES_VC_EMAIL}" ]; then
        export GIT_AUTHOR_NAME="${DOTFILES_VC_NAME}"
        export GIT_AUTHOR_EMAIL="${DOTFILES_VC_EMAIL}"
        export GIT_COMMITTER_NAME="${DOTFILES_VC_NAME}"
        export GIT_COMMITTER_EMAIL="${DOTFILES_VC_EMAIL}"
    fi

    if [ -f "${HOME}/system/bin/git" ] && [ ! -e "${HOME}/system/bin/git-merge" ]; then
        ln -s "${HOME}/system/bin/git" "${HOME}/system/bin/git-merge"
    fi
elif [ "${DOTFILES_OS}" = 'cygwin' ]; then
    # Make password entry work
    if [ -z "${SSH_TTY}" ]; then
        export SSH_TTY
        SSH_TTY="$(/bin/tty.exe)"
    fi

    export GIT_SSH="${PACKAGE_CONF_DIR}/cygwin-auto-ssh-agent"
fi


# Git config complains on Android if multiple copies try to run at once; protect it with a mutex
MUTEX="${HOME}/.dotfiles-gitconfig-mutex"
while ! mkdir "${MUTEX}" >/dev/null 2>/dev/null; do
    sleep 1
done

if which git >/dev/null; then
    if [ ! -z "${DOTFILES_VC_NAME}" ] && [ ! -z "${DOTFILES_VC_EMAIL}" ]; then
        git config --global user.name "${DOTFILES_VC_NAME}"
        git config --global user.email "${DOTFILES_VC_EMAIL}"
    fi

    git config --global color.ui auto
    git config --global core.autocrlf false
    git config --global pager.status true
    git config --global status.showUntrackedFiles all

    if ([ "${DOTFILES_OS}" = 'linux' ] && [ "${DOTFILES_LINUX_VARIANT}" = 'pi' ]) || [ "${DOTFILES_OS}" = 'android' ]; then
        # Git on the pi and on android is too old to support simple
        git config --global push.default matching
    else
        git config --global push.default simple
    fi

    # More Cygwin password entry stuff
    if [ "${DOTFILES_OS}" = 'cygwin' ]; then
        git config --global core.askpass "${PACKAGE_CONF_DIR}/cygwin-askpass"
    fi
fi

rmdir "${MUTEX}"
unset MUTEX


if [ ! -z "${DOTFILES_VC_NAME}" ] && [ ! -z "${DOTFILES_VC_EMAIL}" ]; then
    cat > "${DOTFILES_HG_USERNAME}" <<EOF
# Automatically generated file - do not edit.
# To change username, set the environment variables
# DOTFILES_VC_NAME and DOTFILES_VC_EMAIL appropriately.

[ui]
username = ${DOTFILES_VC_NAME} <${DOTFILES_VC_EMAIL}>
EOF
else
    touch "${DOTFILES_HG_USERNAME}"
fi

if [ ! -e "${DOTFILES_HG_LOCAL}" ]; then
    touch "${DOTFILES_HG_LOCAL}"
fi

if [ ! -z "${DOTFILES_BITBUCKET_USERNAME}" ]; then
    export BITBUCKET="git@bitbucket.org:${DOTFILES_BITBUCKET_USERNAME}"
    export BITBUCKET_HTTPS="https://${DOTFILES_BITBUCKET_USERNAME}@bitbucket.org/${DOTFILES_BITBUCKET_USERNAME}"
fi

if [ ! -z "${DOTFILES_GITHUB_USERNAME}" ]; then
    export GITHUB="git@github.com:${DOTFILES_GITHUB_USERNAME}"
    export GITHUB_HTTPS="https://github.com/${DOTFILES_GITHUB_USERNAME}"
fi

if [ ! -z "${DOTFILES_STARTED_SSH_AGENT}" ]; then
    export GIT_SSH='ssh-auto-add'
fi

export DOTFILES_HG_USERNAME="${HOME}/.dotfiles-hg-username"
export DOTFILES_HG_LOCAL="${HOME}/.dotfiles-hg-local"

dotfiles-home-link "${PACKAGE_SOURCE_DIR}/hgrc"

# Git config complains if multiple copies try to run at once; protect it with a mutex
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
    git config --global push.default simple
    git config --global log.mailmap true
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
    export GIT_SSH='dotfiles-ssh-auto-add'
fi

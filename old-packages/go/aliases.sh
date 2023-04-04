if ! gvm list | grep "${DOTFILES_GVM_GO_VERSION}" >/dev/null 2>/dev/null; then
    gvm install "${DOTFILES_GVM_GO_VERSION}" --prefer-binary &&
    gvm use "${DOTFILES_GVM_GO_VERSION}"
fi

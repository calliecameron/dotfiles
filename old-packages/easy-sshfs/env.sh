export DOTFILES_SSH_MOUNT_DIR="${HOME}/ssh-mounts"

# Cleanup old sshfs directories
if [ -e "${DOTFILES_SSH_MOUNT_DIR}" ]; then
    rmdir --ignore-fail-on-non-empty "${DOTFILES_SSH_MOUNT_DIR}"/* >/dev/null 2>/dev/null
    rmdir --ignore-fail-on-non-empty "${DOTFILES_SSH_MOUNT_DIR}" >/dev/null 2>/dev/null
fi

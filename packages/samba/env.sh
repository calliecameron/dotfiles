if [ ! -e "${HOME}/samba-mounts" ] && [ -d "/run/user/$(id -u)/gvfs" ]; then
    ln -s "/run/user/$(id -u)/gvfs" "${HOME}/samba-mounts"
fi

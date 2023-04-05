# shellcheck shell=sh

if [ "$(umask)" != '077' ]; then
    dotfiles-log-package-problem "Umask is incorrect (want 077, got $(umask)). 'dotfiles-package-install 02-system-umask' to fix."
fi

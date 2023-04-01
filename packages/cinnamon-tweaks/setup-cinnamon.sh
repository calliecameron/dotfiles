#!/bin/bash

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)" &&
export PATH="${THIS_DIR}/bin:${PATH}" &&

CHROME_DESKTOP='google-chrome.desktop'
CHROME_COMMAND='google-chrome'

# Menu
cinnamon-favourites remove mintinstall.desktop &&
cinnamon-favourites remove xchat.desktop &&
cinnamon-favourites remove firefox.desktop &&
cinnamon-favourites add "${CHROME_DESKTOP}" &&
cinnamon-favourites add emacs-daemon.desktop &&
cinnamon-favourites add vlc.desktop &&
cinnamon-favourites add gnucash.desktop &&
cinnamon-favourites add code.desktop &&


# Desktop icons
gsettings set org.nemo.desktop computer-icon-visible true &&
gsettings set org.nemo.desktop home-icon-visible true &&
gsettings set org.nemo.desktop network-icon-visible true &&
gsettings set org.nemo.desktop trash-icon-visible true &&
gsettings set org.nemo.desktop volumes-visible true &&
gsettings set org.nemo.desktop show-desktop-icons true &&

xdg-desktop-icon install --novendor "/usr/share/applications/${CHROME_DESKTOP}" &&
xdg-desktop-icon install --novendor '/usr/share/applications/org.gnome.Terminal.desktop' || exit 1
if [ -f '/usr/share/applications/code.desktop' ]; then
    xdg-desktop-icon install --novendor '/usr/share/applications/code.desktop' || exit 1
fi

# Panel
cinnamon-panel remove firefox.desktop &&
cinnamon-panel add "${CHROME_DESKTOP}" &&
cinnamon-panel add emacs-daemon.desktop &&


# Calendar
gsettings set org.cinnamon.desktop.interface clock-show-date true &&


# Keyboard
gsettings-remove-from-list org.cinnamon.desktop.keybindings.wm push-snap-up '<Control><Super>Up' &&
gsettings-add-if-not-in-list org.cinnamon.desktop.keybindings.wm maximize '<Control><Super>Up' &&
gsettings-remove-from-list org.cinnamon.desktop.keybindings.wm push-tile-up '<Super>Up' &&
gsettings-remove-from-list org.cinnamon.desktop.keybindings.wm push-tile-down '<Super>Down' &&
gsettings-remove-from-list org.cinnamon.desktop.keybindings.wm push-tile-left '<Super>Left' &&
gsettings-remove-from-list org.cinnamon.desktop.keybindings.wm push-tile-right '<Super>Right' &&
gsettings-remove-from-list org.cinnamon.desktop.keybindings.media-keys video-outputs '<Super>p' &&
gsettings-remove-from-list org.cinnamon.desktop.keybindings.media-keys video-rotation-lock '<Super>o' &&


cinnamon-keybinding 'Emacs' 'emacs-daemon' '<Primary><Alt>e' &&
cinnamon-keybinding 'Chrome' "${CHROME_COMMAND}" '<Primary><Alt>b' &&
cinnamon-keybinding 'Chrome Extra' "${CHROME_COMMAND}" 'HomePage' &&
cinnamon-keybinding 'VLC' 'vlc-wrapper' '<Primary><Alt>m' &&
cinnamon-keybinding 'VLC Pause' 'vlc-pause' '<Primary><Alt>space' &&
cinnamon-keybinding 'VLC Next' 'vlc-next' '<Primary><Alt>period' &&
cinnamon-keybinding 'VLC Previous' 'vlc-prev' '<Primary><Alt>comma' &&
cinnamon-keybinding 'Nemo' 'nemo' '<Primary><Alt>h' &&
cinnamon-keybinding 'GnuCash' 'gnucash' '<Primary><Alt>g' &&
cinnamon-keybinding 'Calculator' 'gnome-calculator' '<Primary><Alt>c' &&
cinnamon-keybinding 'Visual Studio Code' 'code' '<Primary><Alt>v' &&

gsettings-add-if-not-in-list org.gnome.libgnomekbd.keyboard options $'ctrl\tctrl:nocaps' || exit 1


# Sound effects
function disable-sound() {
    gsettings set org.cinnamon.sounds "${1}" 'false'
}

disable-sound close-enabled &&
disable-sound maximize-enabled &&
disable-sound switch-enabled &&
disable-sound unplug-enabled &&
disable-sound map-enabled &&
disable-sound plug-enabled &&
disable-sound unmaximize-enabled &&
disable-sound login-enabled &&
disable-sound minimize-enabled &&
disable-sound tile-enabled &&
disable-sound logout-enabled

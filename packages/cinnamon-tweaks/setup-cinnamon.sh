#!/bin/bash

source "${DOTFILES_BASH_COMMON}" &&
export PATH="${THIS_DIR}/bin:${PATH}" &&

if is64bit -q; then
    CHROME_DESKTOP='google-chrome.desktop'
    CHROME_COMMAND='google-chrome'
else
    CHROME_DESKTOP='chromium-browser.desktop'
    CHROME_COMMAND='chromium-browser'
fi

# Menu
cinnamon-favourites remove mintinstall.desktop &&
cinnamon-favourites remove xchat.desktop &&
cinnamon-favourites remove firefox.desktop &&
cinnamon-favourites add "${CHROME_DESKTOP}" &&
cinnamon-favourites add emacs-daemon.desktop &&
cinnamon-favourites add vlc.desktop &&
cinnamon-favourites add thunderbird.desktop &&
cinnamon-favourites add gnucash.desktop &&


# Desktop icons
gsettings set org.nemo.desktop computer-icon-visible true &&
gsettings set org.nemo.desktop home-icon-visible true &&
gsettings set org.nemo.desktop network-icon-visible true &&
gsettings set org.nemo.desktop trash-icon-visible true &&
gsettings set org.nemo.desktop volumes-visible true &&
gsettings set org.nemo.desktop show-desktop-icons true &&

xdg-desktop-icon install --novendor "/usr/share/applications/${CHROME_DESKTOP}" &&
xdg-desktop-icon install --novendor '/usr/share/applications/thunderbird.desktop' &&
xdg-desktop-icon install --novendor '/usr/share/applications/gnome-terminal.desktop' &&


# Panel
cinnamon-panel remove firefox.desktop &&
cinnamon-panel add "${CHROME_DESKTOP}" &&
cinnamon-panel add emacs-daemon.desktop &&
cinnamon-panel add thunderbird.desktop &&


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


cinnamon-keybinding 'Emacs' 'emacs-daemon' '<Primary><Alt>e' &&
cinnamon-keybinding 'Chrome' "${CHROME_COMMAND}" '<Primary><Alt>b' &&
cinnamon-keybinding 'Chrome Extra' "${CHROME_COMMAND}" 'HomePage' &&
cinnamon-keybinding 'VLC' 'vlc-wrapper' '<Primary><Alt>v' &&
cinnamon-keybinding 'VLC Pause' 'vlc-pause' '<Primary><Alt>space' &&
cinnamon-keybinding 'VLC Next' 'vlc-next' '<Primary><Alt>period' &&
cinnamon-keybinding 'VLC Previous' 'vlc-prev' '<Primary><Alt>comma' &&
cinnamon-keybinding 'Nemo' 'nemo' '<Primary><Alt>h' &&
cinnamon-keybinding 'Python' 'gnome-terminal -x ipython3' '<Primary><Alt>p'
cinnamon-keybinding 'Thunderbird' 'thunderbird' '<Primary><Alt>m' &&
cinnamon-keybinding 'GnuCash' 'gnucash' '<Primary><Alt>g' &&
cinnamon-keybinding 'Calculator' 'gnome-calculator' '<Primary><Alt>c' &&
cinnamon-keybinding 'Anki' "anki -b ${HOME}/Anki" '<Primary><Alt>a' &&
cinnamon-keybinding 'Switch monitor' 'monitor_switch.sh' '<Primary><Super>m' &&
cinnamon-keybinding 'Workrave' 'workrave' '<Primary><Alt>w' &&

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

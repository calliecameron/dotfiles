Android Terminal IDE
====================

Make sure Terminal IDE, SL4A, and Python for Android are installed,
and that the tablet is rooted.

Git in Terminal IDE only works with SSH, not https, so the device's
key must be on github. Generating a key is different from usual:

    mkdir -p ~/.ssh
    dropbearkey -t rsa -f ~/.ssh/id_rsa
    dropbearkey -y -f ~/.ssh/id_rsa | grep "^ssh-rsa" >> my_key

Copy contents of `my_key` to github, and `rm my_key`.

To clone, manually copy 'packages/vc/android-ssh-with-key' to the home
directory on the tablet (manually because the repo isn't cloned
yet!). Then:

    cd
    export GIT_SSH="$(pwd)/android-ssh-with-key"
    chmod u+x android-ssh-with-key
    git clone git@github.com:CallumCameron/dotfiles.git .dotfiles
    rm android-ssh-with-key

Then reboot the terminal, and hopefully everything should work.

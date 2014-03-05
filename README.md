This is my config file for various applications.

## Applications
- awesome, the window manager
- curl,
- emacs
- git
- gtk, theme
- shells, including csh/bash/zsh
- tmux
- vim
- xdg, including key remapping, xinitrc etc.
- xscreensaver
- xterm

### Fonts ###
The following application rely on fonts
- awesome -> Monaco
- gtk     -> Lucida Grande
- xterm   -> Monaco

### GTK theme ###
- gtk2-engines-oxygen, oxygen theme for gtk2 applications, such as Google Chrome
- gtk3-engines-oxygen, oxygen theme for gtk3 applications, such as evince, gedit
- kde-style-oxygen, oxygen theme for qt applications

## Notes

Compile and install emacs from source:

    $ ./configure --prefix=$HOME/.usr --with-gif=no --with-x-toolkit=lucid
    $ make && make install

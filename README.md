This is my config file for various applications.

## Applications
- curl,
- emacs
- git
- shells, including csh/bash/zsh
- tmux
- vim
- xdg, including key remapping, xinitrc etc.
- xterm

### Fonts ###
The following application rely on fonts
- xterm   -> Monaco

## Notes

Compile and install emacs from source:

    $ ./configure --prefix=$HOME/.usr --with-gif=no --with-x-toolkit=lucid
    $ make && make install


## For zsh
First install oh-my-zsh to $HOME/.oh-my-zsh, also need Python 2 virtualenvwrapper installed globally.

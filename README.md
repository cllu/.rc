This is my config file for various applications.

## Dependencies ##
- python2

### Fonts ###
The following application rely on fonts
- awesome -> Monaco
- gtk     -> Lucida Grande
- xterm   -> Monaco

The fonts repository is located at [here](https://cllu@bitbucket.org/cllu/fonts.git)

### GTK theme ###
- gtk2-engines-oxygen, oxygen theme for gtk2 applications, such as Google Chrome
- gtk3-engines-oxygen, oxygen theme for gtk3 applications, such as evince, gedit
- kde-style-oxygen, oxygen theme for qt applications


## TODO ##
- add kde config here

Emacs

Compile and install:

    ./configure --prefix=$HOME/.usr --with-gif=no --with-x-toolkit=lucid
    make && make install

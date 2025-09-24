# On OSX, I install things using Homebrew
eval "$(/opt/homebrew/bin/brew shellenv)"

# some applications require valid $JAVA_HOME
if [ -f "/usr/lib/exec/java_home" ]; then
  export JAVA_HOME=$(/usr/libexec/java_home)
fi

# The OSX way for ls colors.
export CLICOLOR=1
export LSCOLORS="gxfxcxdxbxegedabagacad"

# Homebrew keep stuff in /usr/local/ directory
export MANPATH="/usr/local/man:$MANPATH"

# coreutils provides GNU utilities
PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH"

# OSX does not set LANG and LC_ALL by default,
# however, they are required by some app like IPython
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

## Google Cloud SDK is manually installed to /opt folder
export PATH="/opt/google-cloud-sdk/bin:$PATH"

## Go AppEngine
export GOPATH=$HOME/go
export PATH="$GOPATH/bin:$PATH"

## Required to install node-canvas
# see https://github.com/Homebrew/homebrew/issues/14123
export PKG_CONFIG_PATH=/opt/X11/lib/pkgconfig

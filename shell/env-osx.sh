# On OSX, I install things using Homebrew

# some applications require valid $JAVA_HOME
export JAVA_HOME=$(/usr/libexec/java_home)

# The OSX way for ls colors.
export CLICOLOR=1
export LSCOLORS="gxfxcxdxbxegedabagacad"

# Homebrew keep stuff in /usr/local/ directory
export PATH="/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"
export MANPATH="/usr/local/man:$MANPATH"

# coreutils provides GNU utilities
PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH"

# OSX does not set LANG and LC_ALL by default,
# however, they are required by some app like IPython
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

## When rbenv is installed, use it
# Why choose rbenv over rvm: https://github.com/sstephenson/rbenv/wiki/Why-rbenv%3F
if command -v rbenv >/dev/null 2>&1; then
  eval "$(rbenv init -)"
  command -v rbenv >/dev/null 2>&1 && eval "$(rbenv init -)"
fi

## Android SDK
export ANDROID_HOME=/usr/local/opt/android-sdk

## Go AppEngine
export PATH="/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/bin:$PATH"
export GOPATH=$HOME/go
export PATH="$GOPATH/bin:$PATH"

## Required to install node-canvas
# see https://github.com/Homebrew/homebrew/issues/14123
export PKG_CONFIG_PATH=/opt/X11/lib/pkgconfig

# brew Python3
VIRTUALENVWRAPPER_PYTHON=/usr/local/bin/python3

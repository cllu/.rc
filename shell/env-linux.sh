# On linux, I usually work on servers that I do not have root access. Thus I have kept a habit
# to install stuff in my home directory.

# dynamically change the xterm title
case $TERM in
  xterm*)
    precmd () {print -Pn "\e]0;%n@%m\a"}
    ;;
  screen*)
    precmd(){
      # Restore tmux-title to 'zsh'
      printf "\033kzsh\033\\"
      # Restore urxvt-title to 'zsh'
      print -Pn "\e]2;zsh:%~\a"
    }
    preexec(){
      # set tmux-title to running program
      printf "\033k$(echo "$1" | cut -d" " -f1)\033\\"
      # set urxvt-title to running program
      print -Pn "\e]2;zsh:$(echo "$1" | cut -d" " -f1)\a"
    }
    ;;
esac

## PATH
export USR="$HOME/.usr"

# custom binary file path
export PATH="$PATH:$HOME/.bin"
export PATH="$PATH:$USR/bin"

# include/lib/man
export C_INCLUDE_PATH="$USR/include:$C_INCLUDE_PATH"
export CPLUS_INCLUDE_PATH="$USR/include:$CPLUS_INCLUDE_PATH"
export LD_LIBRARY_PATH="$USR/lib:/usr/lib64:/usr/lib:/usr/local/lib:$LD_LIBRARY_PATH"
export MANPATH="$USR/share/man:$MANPATH"

#### JAVA
# set JAVA_HOME
java_home_alt=(
    "/opt/java"        # archlinux aur
    "/usr/java/latest" # centos
    "$USR/opt/java"    # custom installation
)
for dir in $java_home_alt; do
    [[ -d $dir ]] && export JAVA_HOME=$dir && break;
done
# ant and maven
[[ -d $USR/opt/apache-ant  ]] && export ANT_HOME="$USR/opt/apache-ant"
[[ -d $USR/opt/apache-maven ]] && export M2_HOME="$USR/opt/apache-maven"
export MAVEN_OPTS="-Xmx4096M -Xss1024M -XX:MaxPermSize=4096M -XX:+CMSClassUnloadingEnabled"

# Python virtualenv, we prefer to user python3
[[ -d $USR/opt/py2env ]] && export PATH="$USR/opt/py2env/bin:$PATH"
[[ -d $USR/opt/py3env ]] && export PATH="$USR/opt/py3env/bin:$PATH"
# Python NLTK data
export NLTK_DATA="$USR/share/nltk_data/"

# Ruby
export PATH="$HOME/.gem/ruby/2.1.0/bin:$PATH"
## Grunt would like to know where my Chrome is
alias google-chrome=google-chrome-stable
export CHROME_BIN=/usr/bin/google-chrome-stable

# Perl
export PERL_LOCAL_LIB_ROOT="$PERL_LOCAL_LIB_ROOT:$USR/opt/perl5"
export PERL_MB_OPT="--install_base $USR/opt/perl5"
export PERL_MM_OPT="INSTALL_BASE=$USR/opt/perl5"
export PERL5LIB="$USR/opt/perl5/lib/perl5:$PERL5LIB"
export PATH="$USR/opt/perl5/bin:$PATH"

# Android SDK on archlinux
export PATH="/opt/android-sdk/tools:/opt/android-sdk/platform-tools/:$PATH"

# Google Cloud SDK
export CLOUDSDK_PYTHON="/usr/bin/python2"
export PATH="/home/cllu/.usr/opt/google-cloud-sdk/bin:$PATH"
# gsutil
export PATH="$USR/opt/google/gsutil:$PATH"

# pip user directory
PIP_USER_BIN=$HOME/.local/bin
if [ -d $PIP_USER_BIN ]; then
  export PATH=$PIP_USER_BIN:$PATH
fi

# pyenv is installed in home directory by default
if [ -f $HOME/.pyenv/bin/pyenv ]; then
  # the initialization in ./zshrc file (for both macOS and linux)
  PATH="$HOME/.pyenv/bin:$PATH"
fi

# leave background jobs alone
setopt NO_HUP

# oh-my-zsh is a Git submodule in vendor/oh-my-zsh directory
export ZSH=$RCHOME/vendor/oh-my-zsh

# Custom oh-my-zsh stuff
ZSH_CUSTOM=$RCHOME/shell/zsh_custom

# Set name of the theme to load.
ZSH_THEME="robbyrussell"


# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# disable bi-weekly auto-update checks, we will manually upgrade the Git submodule
DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Path to your oh-my-zsh installation.
plugins=(git docker docker-compose kubectl yarn zsh-autosuggestions)

# Use vim keybindings
#bindkey -v

# Keep 1000 lines of history within the shell and save it to ~/.zsh_history:
HISTSIZE=100000
SAVEHIST=100000
HISTFILE=~/.zsh_history

source $ZSH/oh-my-zsh.sh


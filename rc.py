#!/usr/bin/env python

from __future__ import print_function

import os
import sys
import shutil
from time import gmtime, strftime

HOME = os.environ['HOME']
RCHOME = os.path.join(HOME, '.rc')
backup_dir = os.path.join(RCHOME, '.backup' + strftime("_%b%d_%H:%M:%S", gmtime()))

app_config = {
    'awesome': {
        '$HOME/.config/awesome': '$RCHOME/awesome'
    },
    'bash': {
        '$HOME/.bashrc': '$RCHOME/shell/bashrc',
        "$HOME/.aliases": "$RCHOME/shell/aliases",
    },
    'bin': {
        '$HOME/.bin': '$RCHOME/bin'
    },
    'csh': {
        # used to get rid of csh in seis machines.
        '$HOME/.login': '$RCHOME/shell/login',
        '$HOME/.cshrc': '$RCHOME/shell/cshrc'
    },
    'curl': {
        '$HOME/.curlrc': '$RCHOME/curl/curlrc'
    },
    'emacs': {
        '$HOME/.emacs.d': '$RCHOME/emacs'
    },
    'git': {
        '$HOME/.gitconfig': '$RCHOME/git/gitconfig'
    },
    'gtk': {
        '$HOME/.gtkrc-2.0': '$RCHOME/gtk/gtkrc'
    },
    'R': {
        '$HOME/.Renviron': '$RCHOME/R/Renviron'
    },
    'ssh': {
        # note that .ssh folder and the config file should have strict permissions.
        '$HOME/.ssh': '$RCHOME/ssh'
    },
    'tmux': {
        '$HOME/.tmux.conf': '$RCHOME/tmux/tmux.conf'
    },
    'vim': {
        '$HOME/.vimrc': '$RCHOME/vim/vimrc',
        '$HOME/.vim': '$RCHOME/vim'
    },
    'xdg': {
        '$HOME/.xinitrc': '$RCHOME/xdg/xinitrc',
        '$HOME/.xprofile': '$RCHOME/xdg/xprofile',
        '$HOME/.Xmodmap': '$RCHOME/xdg/Xmodmap',
        # there is no consistent implementation currently
        #'$HOME/.local/share/applications/mimeapps.list': '$RCHOME/xdg/mimeapps.list',
        #'$HOME/.local/share/applications/defaults.list': '$RCHOME/xdg/defaults.list',
        # for login managers such as GDM, which will ignore .xinitrc
        #'$HOME/.xsession': '$RCHOME/xinit/xinitrc',
    },
    'xscreensaver': {
        '$HOME/.xscreensaver': '$RCHOME/xscreensaver/xscreensaver'
    },
    'xterm': {
        '$HOME/.Xresources': '$RCHOME/xterm/Xresources'
    },
    'zsh': {
        "$HOME/.zshrc": "$RCHOME/shell/zshrc",
        "$HOME/.aliases": "$RCHOME/shell/aliases",
    },
}

def remove(path):
    found = False

    if os.path.islink(path):
        print(path, "link has already been there, just delete it.")
        os.remove(path)
        found = True
        return
    elif os.path.isfile(path):
        print(path, "file has already been there.")
        found = True
    elif os.path.isdir(path):
        print(path, "folder has already been there.")
        found = True
    else:
        print(path, "not found.")

    if found:
        print("move it to", backup_dir)
        if not os.path.isdir(backup_dir):
            os.makedirs(backup_dir)
        shutil.move(path, backup_dir)

def is_installed(app):
    if app not in app_config.keys():
        return False

    installed = True
    for path, rcfile in app_config[app].items():
        path = path.replace('$HOME', HOME)
        rcfile = rcfile.replace('$RCHOME', RCHOME)

        if os.path.realpath(path) != rcfile:
            installed = False

    return installed


def install(apps):
    for app in apps:
        if is_installed(app):
            print(app, 'is already installed, skip.')
            continue

        if app in app_config.keys():
            print("configuring ", app)
        else:
            print(app, 'configuration not found, continuing...')
            continue

        for path,rcfile in app_config[app].items():
            path = path.replace('$HOME', HOME)
            rcfile = rcfile.replace('$RCHOME', RCHOME)

            remove(path)
            os.symlink(rcfile, path)

def check_status(apps):
    for app in apps:
        installed = is_installed(app)
        print('%15s is%s installed' % (app, ' not' if not installed else ''))

def main():
    if len(sys.argv) < 2:
        sys.exit("rc.py status|install[ all|<app list>]")

    if sys.argv[1] == 'install':
        if sys.argv[2] == 'all':
            apps = app_config.keys()
        else:
            apps = sys.argv[1:]
        install(apps)
    elif sys.argv[1] == 'status':
        apps = app_config.keys()
        check_status(apps)


if __name__ == '__main__':
    main()

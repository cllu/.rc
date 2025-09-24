#!/usr/bin/env python3

from __future__ import print_function

import os
import sys
import shutil
from time import gmtime, strftime

HOME = os.environ['HOME']
RCHOME = os.path.join(HOME, '.rc')
backup_dir = os.path.join(RCHOME, '.backup' + strftime("_%b%d_%H:%M:%S", gmtime()))

app_config = {
    'bash': {
        '$HOME/.bashrc': '$RCHOME/shell/bashrc'
    },
    'bin': {
        '$HOME/.bin': '$RCHOME/bin'
    },
    'curl': {
        '$HOME/.curlrc': '$RCHOME/curl/curlrc'
    },
    'git': {
        '$HOME/.gitconfig': '$RCHOME/git/gitconfig',
        '$HOME/.config/git/ignore': '$RCHOME/git/ignore'
    },
    'tmux': {
        '$HOME/.tmux.conf': '$RCHOME/tmux/tmux.conf'
    },
    'vim': {
        '$HOME/.vimrc': '$RCHOME/vim/vimrc',
        '$HOME/.vim': '$RCHOME/vim'
    },
    'zsh': {
        "$HOME/.zshrc": "$RCHOME/shell/zshrc"
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
    """Check if a given config is installed"""
    if app not in app_config.keys():
        return False

    for path, rcfile in app_config[app].items():
        path = path.replace('$HOME', HOME)
        rcfile = rcfile.replace('$RCHOME', RCHOME)

        if os.path.realpath(path) != rcfile:
            return False

    return True


def install(apps):
    """Install configs"""
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
            try:
                os.makedirs(os.path.dirname(path))
            except OSError:
                pass
            os.symlink(rcfile, path)

def check_status(apps):
    for app in apps:
        installed = is_installed(app)
        print('%15s is%s installed' % (app, ' not' if not installed else ''))

def main():
    if len(sys.argv) < 2:
        sys.exit("rc.py status|install[ all|<app list>]")

    if sys.argv[1] in ('install', 'i'):
        if sys.argv[2] == 'all':
            apps = app_config.keys()
        else:
            apps = sys.argv[1:]
        install(apps)
    elif sys.argv[1] in ('status', 's'):
        apps = app_config.keys()
        check_status(apps)


if __name__ == '__main__':
    main()

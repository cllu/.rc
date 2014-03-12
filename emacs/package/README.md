This folder contains third-party packages.
Instead of using git submodule, I would like to manage package files by myself.

- tomorrow-theme/
  - master branch at [github](https://github.com/chriskempson/tomorrow-theme),
  - commit 376a59 retrieved at Nov 14, 2012.
- evil/
  - master branch at [gitorious](http://gitorious.org/evil/evil),
  - commit 801d3c7, retrieved at Nov 14, 2012 with COPYING doc/ lib/ removed.
- lua-mode.el
  - master branch at [github](http://github.com/immerrr/lua-mode),
  - commit e6a46e, retrieved at Nov 15, 2012.
- markdown-mode.el
  - master branch at [jblevins.org](git://jblevins.org/git/markdown-mode.git),
  - commit c3f865, version 1.8.1, retrieved at Nov 15, 2012.
- php-mode.el
  - get this file from nxhtml package at [launchpad](https://code.launchpad.net/~nxhtml/nxhtml/main),
  - version 1.5.0-nxhtml-1.94, retrieved at Nov 15, 2012.
- auctex
  - release 11.87, retrieved at Dec 09, 2012
- yasnippet
  - master branch at [github](https://github.com/capitaomorte/yasnippet),
  - commit 4f99f9, retrieved at Dec 16, 2012. I only retrieve this main file only.
- dropdown-list.el
  - same with yasnippet.
- snippets
  - selected snippets from yasnippet.
- yaml-mode
  - master branch at [github](https://github.com/yoshiki/yaml-mode),
  - commit e2befd, retrieved at Jan 12, 2013
- editor-config
  - master branch at [github](https://github.com/editorconfig/editorconfig-emacs)
  - commit 2a1da2, retrieved at Mar 12, 2014

In order to make tomorrow-theme to work with Emacs 23, I also include the color-theme package in tomorrow-theme.

### AucTex
auctex need to be compiled

    cd ~/.emacs.d/package/auctex
	./configure
	make

Don't use `make install`, just leave the files there.

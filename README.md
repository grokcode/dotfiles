This repo contains machine-agnostic dotfiles and some extra files for setting up emacs on OS X.

Getting Started
---
1. Clone the repo
2. Symlink each file that you wish to use so that the dotfiles in your home directory point to the dotfiles from this repo. For example:

```
    ln -s /location/of/the/repo/.bashrc ~/.bashrc
```

Emacs on OS X
----
The following install commands along with the included .bashrc can gracefully handle emacs as the git editor and will create and open files if they don't already exist.

1. `brew install --cask emacs`
1. Run emacs with the `emacs` command

In order to give new emacs windows focus:
1. `brew install hammerspoon`
1. `hs` then allow permissions in the hammerspoon gui

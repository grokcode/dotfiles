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
Getting a non-terminal emacs to work well on OS X can be tricky. The following setup can gracefully handle emacs as the git editor and will create and open files if they don't already exist.

1. Install emacs with `brew install emacs --HEAD --use-git-head --cocoa --srgb`
1. `brew services start emacs` to have lannchd run emacs at login.
1. Run emacs with the `emacs` command

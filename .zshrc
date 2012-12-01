#!/usr/bin/zsh
# File: ~/.zshrc
# Author: jess johnson <jess@grokcode.com>

export SHELL='zsh'

# Prompt.
PS1='
%B%F{green}%n@%m% %F{yellow} %~
%b%F{white}$ '

# Emacs FTW.
export EDITOR=emacs
export CVSEDITOR=emacs

# Path.
export PATH=\
/opt/vagrant/bin:\
/home/smudge/bin/WorldOfGoo:\
$HOME/.rvm/bin:\
$PATH:\

# Aliases.
alias mkdir='mkdir -p'
alias emacs='emacs -bg ghostwhite -fg black -cr lightsteelblue  2> /dev/null'
alias dos2unix=fromdos 
alias unix2dos=todos

# More extensive tab completion.
autoload -U compinit
compinit

# Tab completion should be case-insensitive.
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'

# Better completion for killall.
zstyle ':completion:*:killall:*' command 'ps -u $USER -o cmd'

# One history for all open shells; store 10,000 entires. 
HISTFILE=~/.zhistory
HISTSIZE=SAVEHIST=10000
setopt incappendhistory 
setopt sharehistory
setopt extendedhistory

# Superglobs.
setopt extendedglob
unsetopt caseglob

# Virtualenv wrapper
export VIRTUALENVWRAPPER_PYTHON=`which python`
export WORKON_HOME=$HOME/.virtualenv
export PROJECT_HOME=$HOME/work 
source /usr/local/bin/virtualenvwrapper.sh

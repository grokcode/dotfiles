# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# don't put duplicate lines in the history. See bash(1) for more options
# don't overwrite GNU Midnight Commander's setting of `ignorespace'.
HISTCONTROL=$HISTCONTROL${HISTCONTROL+,}ignoredups
# ... or force ignoredups and ignorespace
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

export PS1="\n\e[32;1m${debian_chroot:+($debian_chroot)}\u@\h \e[33;1m\w\e[0m\n$ "

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

export PATH=\
/usr/local/sbin:\
~/bin:\
$PATH:\
.

export CVSROOT=anonymous@amtu.cvs.sourceforge.net:/cvsroot/amtu
export CVS_RSH=ssh
export EDITOR=emacs
export CVSEDITOR=emacs
export JAVA_HOME=$(/usr/libexec/java_home) # OS X

export JPDA_ADDRESS=8000
export JPDA_TRANSPORT=dt_socket

if [ -d "$HOME/Library/Android" ]; then
    export ANDROID_HOME="$HOME/Library/Android/sdk"
    export PATH=$PATH:$HOME/Library/Android/sdk/tools:$HOME/Library/Android/sdk/platform-tools
fi

if [ -d "/usr/local/opt/postgresql@9.6/bin" ]; then
    export PATH=/usr/local/opt/postgresql@9.6/bin:$PATH
fi

if [ -d "/usr/local/opt/node@6/bin" ]; then
    export PATH=$PATH:/usr/local/opt/node@6/bin
fi

# Aliases.
alias mkdir='mkdir -p'
alias gs='git status $1'
alias grep='grep --color=auto $1'
alias pygrep='grep --include \*py --color=auto $1'
if ! hash dos2unix 2>/dev/null; then
    alias dos2unix=fromdos
    alias unix2dos=todos
fi


# enable programmable completion features
# On a new system, `brew install bash-completion` to enable this
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi
if type brew > /dev/null && [ -f `brew --prefix`/etc/bash_completion ]; then
     . `brew --prefix`/etc/bash_completion
fi

# Rbenv shims and autocompletion
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi

# Virtualenv wrapper
export VIRTUALENVWRAPPER_PYTHON=`which python`
export WORKON_HOME=$HOME/.virtualenv
export PROJECT_HOME=$HOME/work
if [ -f /usr/local/bin/virtualenvwrapper.sh ]; then
    source /usr/local/bin/virtualenvwrapper.sh
fi 

# pyenv autocompletion and shims
if which pyenv > /dev/null; then eval "$(pyenv init -)"; fi

# golang setup
export GOPATH=$HOME/work/go
export PATH=$PATH:$GOPATH/bin

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

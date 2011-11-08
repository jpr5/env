##
## Preamble
##

# Load RVM if present
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

GIT_COMP="/opt/local/share/doc/git-core/contrib/completion/git-completion.bash"
if test -f $GIT_COMP; then
    source $GIT_COMP
#    export GIT_PS1_SHOWDIRTYSTATE="1"
#    export GIT_PS1_SHOWUNTRACKEDFILES="1"
#    export GIT_PS1_SHOWUPSTREAM="auto"
fi

case "$TERM" in
    xterm*)
#        PS1='\[\e]0;\u@\h: \w\a\]$(__git_ps1 "(%s) ")\[\033[01;32m\]\u@\h\[\033[00m\](\[\033[01;34m\]\W\[\033[00m\])⇒ '
        PS1='\[\e]0;\u@\h: \w\a\]$(__git_ps1 "⦗%s⦘ ")\[\033[01;32m\]\u@\h\[\033[00m\](\[\033[01;34m\]\W\[\033[00m\])⇒ '
    ;;
    *)
        PS1='\[\e]0;\u@\h: \w\a\]$(__git_ps1 "(%s) ")\u@\h(\W)$ '
esac

##
## Bash options
##

shopt -s cdspell checkwinsize extglob histreedit histappend cmdhist lithist
shopt -s no_empty_cmd_completion

##
## Envariables
##

# MacPorts Installer addition on 2010-02-02_at_16:16:55: adding an appropriate PATH variable for use with MacPorts.
export PATH=/opt/local/libexec/gnubin:/opt/local/bin:/opt/local/sbin:$PATH
# Finished adapting your PATH environment variable for use with MacPorts.

#export HISTCONTROL=ignoredups,ignorespace,ignoreboth
export HISTSIZE=100000

export LSCOLORS="ExfxcxdxbxEgedabagacad"
export TMPDIR=/tmp
export MANPATH=$MANPATH:/usr/local/git/man

export EDITOR="/usr/bin/emacs"
export CVS_EDITOR=$EDITOR SVN_EDITOR=$EDITOR GIT_EDITOR=$EDITOR

# Put the real mysql utils at the beginning of the path.
export PATH=/opt/local/lib/mysql5/bin:$PATH
export PATH=/opt/local/lib/postgresql90/bin:$PATH

##
## Aliases
##

alias su='sudo -s'
alias df='df -klH '
alias rs='rsync -avuxe ssh '
alias grep='grep --line-buffered --color=auto'
alias less='less -R'
alias lsof='lsof -nP'
alias diff='diff -u'
alias cleanup='find -name .#* -o -name *.~*~  | xargs rm'

[[ -n "`which gfind`"  ]] && alias find=gfind

case `uname` in
    Linux)
    alias ls='ls -F --color=auto '
    ;;
    Darwin)
    alias ls='ls -FG '
    ;;
esac

##
## Functions
##

function field { s=""; c=""; for a in $*; do s="$s$c\$$a"; c=","; done; awk "{ print $s }"; }
function fcount {
    field $1 | sort | uniq -c | sort -rn | head $2
}
alias f=field

test -f .profile.cc && source .profile.cc

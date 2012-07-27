# If not running interactively, don't do anything
[ -z "$PS1" ] && return

##
## Setup Prompt
##

GIT_COMP="/opt/local/share/doc/git-core/contrib/completion/git-completion.bash"
if test -f $GIT_COMP; then
    source $GIT_COMP
#    export GIT_PS1_SHOWDIRTYSTATE="1"
#    export GIT_PS1_SHOWUNTRACKEDFILES="1"
#    export GIT_PS1_SHOWUPSTREAM="auto"
    export GIT_MERGE_AUTOEDIT=no
fi

case "$TERM" in
    xterm*)
        PS1='\[\e]0;\u@\h: \w\a\]$(__git_ps1 "⦗%s⦘ ")\[\033[01;32m\]\u@\h\[\033[00m\](\[\033[01;34m\]\W\[\033[00m\])⇒ '
        ;;
    dumb)
        PS1='$(__git_ps1 "(%s) ")\u@\h(\W)⇒ '
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

# Put the real mysql utils at the beginning of the path.
export PATH=/opt/local/lib/mysql5/bin:$PATH
export PATH=/opt/local/lib/postgresql90/bin:$PATH

#export HISTCONTROL=ignoredups,ignorespace,ignoreboth
export HISTSIZE=100000

export LSCOLORS="ExfxcxdxbxEgedabagacad"
export TMPDIR=/tmp

export EDITOR="/Applications/MacPorts/Emacs.app/Contents/MacOS/bin/emacsclient"
export CVS_EDITOR=$EDITOR SVN_EDITOR=$EDITOR GIT_EDITOR=$EDITOR

[[ -x /usr/bin/lesspipe ]] && eval "$(SHELL=/bin/sh lesspipe)"

##
## Aliases
##

alias su='sudo -s'
alias df='df -klH '
alias grep='grep --line-buffered --color=auto'
alias less='less -R'
alias lsof='lsof -nP'
alias diff='diff -u'

[[ -n "`which gfind`" ]] && alias find=gfind

case `uname` in
    Linux)
    alias ls='ls -F --color=auto '
    ;;

    Darwin)
    alias ls='ls -FG '
    alias emacs='open -a Emacs'
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

## EC2
test -f .ec2/rc && source .ec2/rc

export ZSH="$HOME/.zsh"

# DISABLE_AUTO_TITLE="true"
# DISABLE_UNTRACKED_FILES_DIRTY="true"
# CASE_SENSITIVE="true"

HYPHEN_INSENSITIVE="true"
DISABLE_AUTO_UPDATE="true"
COMPLETION_WAITING_DOTS="true"

ZSH_DISABLE_COMPFIX=true
ZSH_THEME="jpr5"

plugins=(ruby rbenv colored-man-pages zsh-syntax-highlighting zsh-autosuggestions)

source $ZSH/oh-my-zsh.sh

export LANG=en_US.UTF-8
export DEFAULT_USER=jpr5

export LESS="-RFX"
export LSCOLORS="ExfxcxdxbxEgedabagacad"
export TMPDIR=/tmp
export EDITOR="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient"
export CVS_EDITOR=$EDITOR SVN_EDITOR=$EDITOR GIT_EDITOR=vim

export PATH=~/.rbenv/shims:$PATH

alias ls='ls -FG'
alias emacs='open -a Emacs'
alias su='sudo -s'
alias df='df -klH '
alias grep='grep --line-buffered --color=auto'
alias less='less -R'
alias lsof='lsof -nP'
alias diff='diff -u'

setopt extendedglob autopushd notify emacs alwaystoend listpacked
unsetopt hist_verify
#alwaystoend autocd autopushd combiningchars completeinword extendedhistory noflowcontrol histexpiredupsfirst histignoredups histignorespace histverify incappendhistory interactive interactivecomments longlistjobs monitor promptsubst pushdignoredups pushdminus sharehistory shinstdin zle

zstyle ':completion:*' completer _complete _match _approximate
zstyle ':completion:*:match:*' original only
zstyle ':completion:*:approximate:*' max-errors 1 numeric
zstyle ':completion:*:functions' ignored-patterns '_*'
zstyle -e ':completion:*:approximate:*' max-errors 'reply=($((($#PREFIX+$#SUFFIX)/3))numeric)'
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*'   force-list always
zstyle ':completion:*:cd:*' ignore-parents parent pwd

function field { s=""; c=""; for a in $*; do s="$s$c\$$a"; c=","; done; awk "{ print $s }"; }
function fcount {
    field $1 | sort | uniq -c | sort -rn | head $2
}
alias f=field

test -f .ec2/rc && source .ec2/rc
ssh-add -l &>/dev/null && [[ $? = 1 ]] && ssh-add -A &>/dev/null || true
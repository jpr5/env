export ZSH="$HOME/.zsh"
export ZDOTDIR=$ZSH

# DISABLE_UNTRACKED_FILES_DIRTY="true"
# CASE_SENSITIVE="true"

DISABLE_AUTO_TITLE="true"
HYPHEN_INSENSITIVE="true"
DISABLE_AUTO_UPDATE="true"
COMPLETION_WAITING_DOTS="true"

ZSH_DISABLE_COMPFIX=true
ZSH_THEME="jpr5"

plugins=(ruby rbenv colored-man-pages zsh-syntax-highlighting zsh-autosuggestions)

source $ZSH/oh-my-zsh.sh

export LANG=en_US.UTF-8
export DEFAULT_USER=jpr5
export TMPDIR=/tmp

export EDITOR="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient"
export CVS_EDITOR=$EDITOR SVN_EDITOR=$EDITOR GIT_EDITOR=$EDITOR

export LESS="-RFX"
export LSCOLORS="ExfxcxdxbxEgedabagacad"
#export WORDCHARS='/~!#$%^&*(){}[]<>?.+;-'

export PATH=~/.rbenv/shims:/usr/local/opt/gnu-tar/libexec/gnubin:$PATH

alias ls='ls -FG'
alias e='open -a Emacs'
alias emacs="$EDITOR -n"
alias su='sudo -s'
alias df='df -klH '
alias grep='grep --line-buffered --color=auto'
alias less='less -R'
alias lsof='lsof -nP'
alias diff='diff -u'

setopt extendedglob autopushd notify alwaystoend listpacked completeinword zle emacs bashautolist
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

function reset_gems() {
    for gem in `gem list --no-versions`; do gem uninstall $gem -aIx; done
    gem install bundler:1.17.2
}

function netkill() {
    kill -9 `lsof -i tcp:$1 | f 1 2 | uniq | tail -1`
}

test -f .ec2/rc && source .ec2/rc
test -f .sdn/rc && source .sdn/rc

ssh-add -l &>/dev/null || ssh-add -A &>/dev/null

[[ -f $EDITOR ]] && [[ -n "$(emacs  -e '(frames-on-display-list)' | grep F1)" ]] && emacs -ce '(make-frame-invisible (selected-frame))' >/dev/null || true

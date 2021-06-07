ZSH_USER="jpr5"
export ZSH=`ls -d ~$ZSH_USER/.zsh`
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

export LESS="-RFX"
export LSCOLORS="ExfxcxdxbxEgedabagacad"
#export WORDCHARS='/~!#$%^&*(){}[]<>?.+;-'
#export OBJC_DISABLE_INITIALIZE_FORK_SAFETY=YES


export PATH=/usr/local/opt/gnu-tar/libexec/gnubin:/opt/homebrew/bin:/usr/local/homebrew/bin:$PATH
export RBENV_ROOT=/usr/local/rbenv
export PATH=$RBENV_ROOT/bin:$PATH
eval "$(rbenv init -)"

case `uname` in
    Darwin)
        export EDITOR="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient"
        alias emacs="$EDITOR -n"
        alias ls='ls -FG'
        alias e='open -a Emacs'
        ;;
    Linux)
        export EDITOR=vim
        alias ls='ls -F --color'
        ;;
esac

export CVS_EDITOR=$EDITOR SVN_EDITOR=$EDITOR GIT_EDITOR=$EDITOR
export OBJC_DISABLE_INITIALIZE_FORK_SAFETY=YES

alias su='sudo -s'
alias df='df -klH '
alias grep='grep --line-buffered --color=auto'
alias diff='diff -u'

setopt extendedglob notify alwaystoend listpacked completeinword zle emacs bashautolist
unsetopt hist_verify autopushd
#alwaystoend autocd autopushd combiningchars completeinword extendedhistory noflowcontrol histexpiredupsfirst histignoredups histignorespace histverify incappendhistory interactive interactivecomments longlistjobs monitor promptsubst pushdignoredups pushdminus sharehistory shinstdin zle

zstyle ':completion:*' completer _complete _match _approximate
zstyle ':completion:*:match:*' original only
zstyle ':completion:*:approximate:*' max-errors 1 numeric
zstyle ':completion:*:functions' ignored-patterns '_*'
zstyle -e ':completion:*:approximate:*' max-errors 'reply=($((($#PREFIX+$#SUFFIX)/3))numeric)'
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*'   force-list always
zstyle ':completion:*:cd:*' ignore-parents parent pwd

bindkey '\ef' emacs-forward-word
bindkey '\eb' emacs-backward-word

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

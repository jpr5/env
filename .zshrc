#
# Zsh-specific options
#

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

plugins=(ruby colored-man-pages zsh-syntax-highlighting zsh-autosuggestions)

source $ZSH/oh-my-zsh.sh

export ZSH_HIGHLIGHT_MAXLENGTH=60

setopt extendedglob notify alwaystoend listpacked completeinword zle emacs bashautolist
unsetopt hist_verify autopushd
#alwaystoend autocd autopushd combiningchars completeinword extendedhistory noflowcontrol histexpiredupsfirst histignoredups histignorespace histverify incappendhistory interactive interactivecomments longlistjobs monitor promptsubst pushdignoredups pushdminus sharehistory shinstdin zle

zstyle ':completion:*' list-suffixes expand prefix suffix
#zstyle ':completion:*' completer _complete _match _approximate
zstyle ':completion:*:match:*' original only
zstyle ':completion:*:approximate:*' max-errors 1 numeric
zstyle ':completion:*:functions' ignored-patterns '_*'
zstyle -e ':completion:*:approximate:*' max-errors 'reply=($((($#PREFIX+$#SUFFIX)/3))numeric)'
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*'   force-list always
zstyle ':completion:*:cd:*' ignore-parents parent pwd

# case-insensitive completion
zstyle ':completion:*' matcher-list 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]} l:|=* r:|=*' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]} l:|=* r:|=*' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]} l:|=* r:|=*' 

bindkey '\ef' emacs-forward-word
bindkey '\eb' emacs-backward-word

#
# Common RC
#

source ~/.commonrc

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

plugins=(ruby colored-man-pages zsh-autosuggestions zsh-syntax-highlighting)

source $ZSH/oh-my-zsh.sh

#source /opt/homebrew/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
#export ZSH_HIGHLIGHT_HIGHLIGHTERS_DIR=/opt/homebrew/share/zsh-syntax-highlighting/highlighters
export ZSH_HIGHLIGHT_MAXLENGTH=60

setopt nobeep
setopt COMPLETE_IN_WORD
unsetopt auto_menu
setopt AUTO_PARAM_SLASH
setopt extendedglob notify alwaystoend listpacked completeinword zle emacs bashautolist
unsetopt autopushd
#alwaystoend autocd autopushd combiningchars completeinword extendedhistory noflowcontrol histexpiredupsfirst histignoredups histignorespace histverify incappendhistory interactive interactivecomments longlistjobs monitor promptsubst pushdignoredups pushdminus sharehistory shinstdin zle
unsetopt nomatch

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
#zstyle ':completion:*' matcher-list 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]} l:|=* r:|=*' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]} l:|=* r:|=*' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]} l:|=* r:|=*'

bindkey '\ef' emacs-forward-word
bindkey '\eb' emacs-backward-word


HISTFILE="$HOME/.zsh_history"
HISTSIZE=10000000
SAVEHIST=10000000
setopt BANG_HIST                 # Treat the '!' character specially during expansion.
setopt EXTENDED_HISTORY          # Write the history file in the ":start:elapsed;command" format.
setopt APPEND_HISTORY            # Write to the history file immediately, not when the shell exits.
unsetopt SHARE_HISTORY            # Share history between all sessions.
setopt HIST_EXPIRE_DUPS_FIRST    # Expire duplicate entries first when trimming history.
setopt HIST_IGNORE_DUPS          # Don't record an entry that was just recorded again.
setopt HIST_IGNORE_ALL_DUPS      # Delete old recorded entry if new entry is a duplicate.
setopt HIST_FIND_NO_DUPS         # Do not display a line previously found.
setopt HIST_IGNORE_SPACE         # Don't record an entry starting with a space.
setopt HIST_SAVE_NO_DUPS         # Don't write duplicate entries in the history file.
setopt HIST_REDUCE_BLANKS        # Remove superfluous blanks before recording entry.
setopt HIST_BEEP                 # Beep when accessing nonexistent history.

## experiment
#autocorrect() {
#  zle .spell-word
#  zle .$WIDGET
#}
#zle -N accept-line autocorrect
#zle -N magic-space autocorrect
#bindkey ' ' magic-space


#
# Common RC
#

source ~/.commonrc

export COLUMNS="120"

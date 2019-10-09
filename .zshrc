# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="/Users/jpr5/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="jpr5"

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in ~/.oh-my-zsh/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to automatically update without prompting.
#DISABLE_UPDATE_PROMPT="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS=true

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
#ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(ruby rbenv)

export ZSH_DISABLE_COMPFIX=true
source $ZSH/oh-my-zsh.sh

# User configuration

export LANG=en_US.UTF-8
export DEFAULT_USER=jpr5

export LESS="-RFX"
export LSCOLORS="ExfxcxdxbxEgedabagacad"
export TMPDIR=/tmp
export EDITOR="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient"
export CVS_EDITOR=$EDITOR SVN_EDITOR=$EDITOR GIT_EDITOR=vim

export PATH=~jpr5/.rbenv/shims:$PATH

alias ls='ls -FG'
alias emacs='open -a Emacs'
alias su='sudo -s'
alias df='df -klH '
alias grep='grep --line-buffered --color=auto'
alias less='less -R'
alias lsof='lsof -nP'
alias diff='diff -u'

#from bash
#shopt -s cdspell checkwinsize extglob histreedit histappend cmdhist lithist
#shopt -s no_empty_cmd_completion

setopt extendedglob autopushd notify emacs
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

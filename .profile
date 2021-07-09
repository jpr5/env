#
# Shell Prompt
#

GIT_COMP="$HOME/.git-prompt.sh"
if test -f $GIT_COMP; then
    source $GIT_COMP
    #export GIT_PS1_SHOWDIRTYSTATE="1"
    #export GIT_PS1_SHOWUNTRACKEDFILES="1"
    #export GIT_PS1_SHOWUPSTREAM="auto"
    export GIT_MERGE_AUTOEDIT=no

    GIT_PROMPT='$(__git_ps1 "⦗%s⦘ ")'
fi

case "$TERM" in
    xterm*)
        [[ "`id -u`" -eq "0" ]] && U='\[\e[1;31m\]\u\[\e[0m\]' || U='\[\e[1;32m\]\u\[\e[0m\]'
        H='\[\e[1;34m\]\h\[\e[0m\]'
        S='\[\e[0;37m\]@\[\e[0m\]'
        D='\[\e[0;37m\]\W\[\e[0m\]'

        TITLEBAR_PROMPT='\[\e]0;\u@\h: \w\a\]'
        ;;
    *)
        U='\u'
        H='\h'
        S='@'
        D='\W'
esac

export PS1=$(echo -e "$TITLEBAR_PROMPT$GIT_PROMPT$U$S$H($D)⇒ ")

#
# Bash-specific options
#

shopt -s cdspell checkwinsize extglob histreedit histappend cmdhist lithist
shopt -s no_empty_cmd_completion

#export HISTCONTROL=ignoredups,ignorespace,ignoreboth
export HISTSIZE=100000
export BASH_SILENCE_DEPRECATION_WARNING=1

#
# Common RC
#

source ~/.commonrc

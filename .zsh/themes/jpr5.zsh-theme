# vim:ft=zsh ts=2 sw=2 sts=2
#
# jpr5's theme, based on agnoster's theme (https://gist.github.com/3712874)
#

typeset -aHg PROMPT_SEGMENT_ORDER=(
    prompt_status
    prompt_git
    prompt_context
    prompt_dir
    prompt_end
)

# Characters
SEGMENT_SEPARATOR=""
PLUSMINUS="±"
BRANCH=""
DETACHED="➦"
CROSS="✘"
LIGHTNING="⚡"
GEAR="⚙" # ⍟ ⎈
EQUAL="═"
RIGHT="➜"
EX="✖" # ✗
ASTERISK="❉" # ✱❉✹
PLUS="✚"
STAR="✭"
UP="↑"
DOWN="↓"
LESSTHAN="❮"
GREATERTHAN="❯"
DOUBLERIGHT="⇒"
SCISSORS="✂"


##
## Utility
##

# Copied from zsh's vcs/git/whatever.zsh.  needed to change the behavior of what
# it emitted.
git_status() {
    local INDEX
    INDEX=$(git status --porcelain -b -- $PWD  2> /dev/null)
    if $(echo "$INDEX" | command grep -E '^\?\? ' &> /dev/null); then
        GIT_STATUS="$ZSH_THEME_GIT_PROMPT_UNTRACKED$GIT_STATUS"
    fi
    if $(echo "$INDEX" | grep '^A  ' &> /dev/null); then
        GIT_STATUS="$ZSH_THEME_GIT_PROMPT_ADDED$GIT_STATUS"
    elif $(echo "$INDEX" | grep '^M  ' &> /dev/null); then
        GIT_STATUS="$ZSH_THEME_GIT_PROMPT_ADDED$GIT_STATUS"
    elif $(echo "$INDEX" | grep '^MM ' &> /dev/null); then
        GIT_STATUS="$ZSH_THEME_GIT_PROMPT_ADDED$GIT_STATUS"
    fi
    if $(echo "$INDEX" | grep '^ M ' &> /dev/null); then
        GIT_STATUS="$ZSH_THEME_GIT_PROMPT_MODIFIED$GIT_STATUS"
    elif $(echo "$INDEX" | grep '^AM ' &> /dev/null); then
        GIT_STATUS="$ZSH_THEME_GIT_PROMPT_MODIFIED$GIT_STATUS"
    elif $(echo "$INDEX" | grep '^MM ' &> /dev/null); then
        GIT_STATUS="$ZSH_THEME_GIT_PROMPT_MODIFIED$GIT_STATUS"
    elif $(echo "$INDEX" | grep '^ T ' &> /dev/null); then
        GIT_STATUS="$ZSH_THEME_GIT_PROMPT_MODIFIED$GIT_STATUS"
    fi
    if $(echo "$INDEX" | grep '^R  ' &> /dev/null); then
        GIT_STATUS="$ZSH_THEME_GIT_PROMPT_RENAMED$GIT_STATUS"
    fi
    if $(echo "$INDEX" | grep '^ D ' &> /dev/null); then
        GIT_STATUS="$ZSH_THEME_GIT_PROMPT_DELETED$GIT_STATUS"
    elif $(echo "$INDEX" | grep '^D  ' &> /dev/null); then
        GIT_STATUS="$ZSH_THEME_GIT_PROMPT_DELETED$GIT_STATUS"
    elif $(echo "$INDEX" | grep '^AD ' &> /dev/null); then
        GIT_STATUS="$ZSH_THEME_GIT_PROMPT_DELETED$GIT_STATUS"
    fi
    if $(command git rev-parse --verify refs/stash >/dev/null 2>&1); then
        GIT_STATUS="$ZSH_THEME_GIT_PROMPT_STASHED$GIT_STATUS"
    fi
    if $(echo "$INDEX" | grep '^UU ' &> /dev/null); then
        GIT_STATUS="$ZSH_THEME_GIT_PROMPT_UNMERGED$GIT_STATUS"
    fi
    if $(echo "$INDEX" | grep '^## [^ ]\+ .*ahead' &> /dev/null); then
        GIT_STATUS="$ZSH_THEME_GIT_PROMPT_AHEAD$GIT_STATUS"
    fi
    if $(echo "$INDEX" | grep '^## [^ ]\+ .*behind' &> /dev/null); then
        GIT_STATUS="$ZSH_THEME_GIT_PROMPT_BEHIND$GIT_STATUS"
    fi
    if $(echo "$INDEX" | grep '^## [^ ]\+ .*diverged' &> /dev/null); then
        GIT_STATUS="$ZSH_THEME_GIT_PROMPT_DIVERGED$GIT_STATUS"
    fi
    print -n "${GIT_STATUS}"
}

find_up () {
  _path=$(pwd)
  while [[ "$_path" != "" && ! -e "$_path/$1" ]]; do
    _path=${_path%/*}
  done
  echo "$_path"
}

##
## Prompt Segments
##

# Status:
# - was there an error
# - are there background jobs?
prompt_status() {
    local symbols=()
    [[ $RETVAL -ne 0 ]] && symbols+="%F{red}$CROSS%f"
    [[ -n "$(jobs)" ]] && symbols+="%F{063}$GEAR%f"
    [[ -n "$symbols" ]] && print -n " $symbols "
}

#ZSH_THEME_GIT_PROMPT_PREFIX="%B%F{white}⦗%f%b"
#ZSH_THEME_GIT_PROMPT_SUFFIX="%B%F{white}⦘%f%b"
#ZSH_THEME_GIT_PROMPT_DIRTY="%F{red}$PLUSMINUS%f" # don't use
ZSH_THEME_GIT_PROMPT_CLEAN=""
ZSH_THEME_GIT_PROMPT_ADDED="%{$fg[green]%}$PLUS"
ZSH_THEME_GIT_PROMPT_MODIFIED="%B%F{white}$ASTERISK"
ZSH_THEME_GIT_PROMPT_DELETED="%{$fg[red]%}$EX"
ZSH_THEME_GIT_PROMPT_RENAMED="%{%F{208}%}$RIGHT"
ZSH_THEME_GIT_PROMPT_UNMERGED="%{$fg[magenta]%}$EQUAL"
ZSH_THEME_GIT_PROMPT_UNTRACKED="%F{yellow}$STAR"

prompt_git() {
    local ref post
    local parencolor refcolor="007" behindaheadcolor="007"

    ref="$vcs_info_msg_0_"
    if [[ -n "$ref" ]] && [[ -n "$(find_up .git)" ]]; then
        if [[ -n "$(git status --porcelain --ignore-submodules -- ${PWD} 2>/dev/null)" ]]; then
            parencolor="red"
            post="$(git_status)";
        else
            parencolor="028"
        fi

        ahead=$(command git rev-list --count ..@{upstream} 2>/dev/null)
        behind=$(command git rev-list --count @{upstream}.. 2> /dev/null)

        [[ "$behind" -gt 0 ]] && behindahead="${UP}${behind}"
        [[ "$ahead" -gt 0 ]] && behindahead="${behindahead}${DOWN}${ahead}"

        print -n "%B%F{$parencolor}${LESSTHAN}%f%b"
        print -n "%F{$refcolor}${ref}%f"
        print -n "${post}%b%f%k"
        print -n "%B%F{$behindaheadcolor}${behindahead}%f%b"
        print -n "%B%F{$parencolor}${GREATERTHAN}%f%b "
    fi
}

# Context: user@hostname
prompt_context() {
    # 208=orange 039=darkercyan  159=prettycyan
    local usercolor='193' atcolor="094" hostcolor="243"
    [[ $UID -eq 0 ]] && usercolor='196'

    print -n "%B%F{$usercolor}$USER%b%F{$atcolor}@%F{$hostcolor}%m%f"
}

# Dir: current working directory
prompt_dir() {
    print -n '%B%F{white}(%f%b%c%B%F{white})%f%b'
}

# End the prompt, closing any open segments
prompt_end() {
    local promptcolor="033"
    print -n "%B%F{$promptcolor}%K{black}${DOUBLERIGHT}%b%f%k"
}

##
## Main
##

prompt_main() {
    RETVAL=$?
    for prompt_segment in "${PROMPT_SEGMENT_ORDER[@]}"; do
        [[ -n $prompt_segment ]] && $prompt_segment
    done
}

prompt_precmd() {
    vcs_info
    PROMPT='%{%f%b%k%}$(prompt_main) '
}

prompt_setup() {
    autoload -Uz add-zsh-hook
    autoload -Uz vcs_info

    add-zsh-hook precmd prompt_precmd

    zstyle ':vcs_info:*' enable git
    zstyle ':vcs_info:*' check-for-changes false
    zstyle ':vcs_info:git*' formats '%b'
    zstyle ':vcs_info:git*' actionformats '%b (%a)'
}

prompt_setup "$@"

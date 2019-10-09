# vim:ft=zsh ts=2 sw=2 sts=2
#
# agnoster's Theme - https://gist.github.com/3712874
# A Powerline-inspired theme for ZSH
#
# # README
#
# In order for this theme to render correctly, you will need a
# [Powerline-patched font](https://gist.github.com/1595572).
#
# In addition, I recommend the
# [Solarized theme](https://github.com/altercation/solarized/) and, if you're
# using it on Mac OS X, [iTerm 2](http://www.iterm2.com/) over Terminal.app -
# it has significantly better color fidelity.
#
# # Goals
#
# The aim of this theme is to only show you *relevant* information. Like most
# prompts, it will only show git information when in a git working directory.
# However, it goes a step further: everything from the current user and
# hostname to whether the last call exited with an error to whether background
# jobs are running in this shell will all be displayed automatically when
# appropriate.

### Segments of the prompt, default order declaration

typeset -aHg AGNOSTER_PROMPT_SEGMENTS=(
    prompt_status
    prompt_git
    prompt_context
    prompt_virtualenv
    prompt_dir
    prompt_end
)

### Segment drawing
# A few utility functions to make it easy and re-usable to draw segmented prompts

CURRENT_BG='NONE'
if [[ -z "$PRIMARY_FG" ]]; then
    PRIMARY_FG=black
fi

# Characters
#SEGMENT_SEPARATOR="\ue0b0"
SEGMENT_SEPARATOR=""
PLUSMINUS="\u00b1"
BRANCH="\ue0a0"
DETACHED="\u27a6"
CROSS="\u2718"
LIGHTNING="\u26a1"
GEAR="\u2699"

# Begin a segment
# Takes two arguments, background and foreground. Both can be omitted,
# rendering default background/foreground.
prompt_segment() {
  local bg fg
  [[ -n $1 ]] && bg="%K{$1}" || bg="%k"
  [[ -n $2 ]] && fg="%F{$2}" || fg="%f"
  if [[ $CURRENT_BG != 'NONE' && $1 != $CURRENT_BG ]]; then
    print -n "%{$bg%F{$CURRENT_BG}%}$SEGMENT_SEPARATOR%{$fg%}"
  else
    print -n "%{$bg%}%{$fg%}"
  fi
  CURRENT_BG=$1
  [[ -n $3 ]] && print -n $3
}

# End the prompt, closing any open segments
prompt_end() {
  if [[ -n $CURRENT_BG ]]; then
    print -n "%{%k%F{$CURRENT_BG}%}$SEGMENT_SEPARATOR"
  else
    print -n "%{%k%}"
  fi
  print -n "%{%f%}"
  CURRENT_BG=''
  print -n "%F{white}%K{black}%B%F{grey}⇒%b%f%k"
}

### Prompt components
# Each component will draw itself, and hide itself if no information needs to be shown

# Context: user@hostname (who am I and where am I)
prompt_context() {
  local user=`whoami`
  local color=green

  [[ $UID -eq 0 ]] && color=red

  prompt_segment $PRIMARY_FG default "%B%F{$color}$user%b%F{white}@%B%F{blue}%m%b"
}

function git_status() {
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

ZSH_THEME_GIT_PROMPT_PREFIX="%B%F{white}⦗%f%b"
ZSH_THEME_GIT_PROMPT_SUFFIX="%B%F{white}⦘%f%b"
ZSH_THEME_GIT_PROMPT_DIRTY="%F{red}$PLUSMINUS%f"
ZSH_THEME_GIT_PROMPT_CLEAN=""
ZSH_THEME_GIT_PROMPT_ADDED="%{$fg[cyan]%}✈"
ZSH_THEME_GIT_PROMPT_MODIFIED="%{$fg[yellow]%}✱"
ZSH_THEME_GIT_PROMPT_DELETED="%{$fg[red]%}✗"
ZSH_THEME_GIT_PROMPT_RENAMED="%{$fg[blue]%}➦"
ZSH_THEME_GIT_PROMPT_UNMERGED="%{$fg[magenta]%}✂"
ZSH_THEME_GIT_PROMPT_UNTRACKED="%F{green}+%f"

is_dirty() {
    test -n "$(git status --porcelain --ignore-submodules -- ${PWD} 2>/dev/null)"
}

#RPROMPT="$(prompt_git)"
#RPROMPT='%F{white}$(git_prompt_info)%f $(git_prompt_status)%f%k%b'

# Git: branch/detached head, dirty status
prompt_git() {
    local ref pre post

    ref="$vcs_info_msg_0_"
    if [[ -n "$ref" ]] && [[ -d "$PWD/.git" ]]; then
        if is_dirty; then
            pre="%F{red}$PLUSMINUS%f"
            post="$(git_status)";
        else
            ref="${ref}"
        fi
        #    if [[ "${ref/.../}" == "$ref" ]]; then
        #      pre="%F{white}$BRANCH%f "
        #    else
        #      ref="%F{white}$DETACHED%f "
        #    fi
        print -n "%F{white}⦗%f${pre}${ref}${post}%F{white}⦘%f "
    fi
}

# Dir: current working directory
prompt_dir() {
  prompt_segment black white '(%c)'
}

# Status:
# - was there an error
# - am I root
# - are there background jobs?
prompt_status() {
  local symbols
  symbols=()
  [[ $RETVAL -ne 0 ]] && symbols+="%{%F{red}%}$CROSS"
#  [[ $UID -eq 0 ]] && symbols+="%{%F{yellow}%}$LIGHTNING"
  [[ $(jobs -l | wc -l) -gt 0 ]] && symbols+="%{%F{cyan}%}$GEAR"

  [[ -n "$symbols" ]] && prompt_segment $PRIMARY_FG default " $symbols "
}

# Display current virtual environment
prompt_virtualenv() {
  if [[ -n $VIRTUAL_ENV ]]; then
    color=cyan
    prompt_segment $color $PRIMARY_FG
    print -Pn " $(basename $VIRTUAL_ENV) "
  fi
}

## Main prompt
prompt_agnoster_main() {
  RETVAL=$?
  CURRENT_BG='NONE'
  for prompt_segment in "${AGNOSTER_PROMPT_SEGMENTS[@]}"; do
    [[ -n $prompt_segment ]] && $prompt_segment
  done
}

prompt_agnoster_precmd() {
  vcs_info
  PROMPT='%{%f%b%k%}$(prompt_agnoster_main) '
}

prompt_agnoster_setup() {
  autoload -Uz add-zsh-hook
  autoload -Uz vcs_info

  prompt_opts=(cr subst percent)

  add-zsh-hook precmd prompt_agnoster_precmd

  zstyle ':vcs_info:*' enable git
  zstyle ':vcs_info:*' check-for-changes false
  zstyle ':vcs_info:git*' formats '%b'
  zstyle ':vcs_info:git*' actionformats '%b (%a)'
}

prompt_agnoster_setup "$@"

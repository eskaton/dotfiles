# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000000
SAVEHIST=1000000
bindkey -v
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '~/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

setopt EXTENDED_GLOB
setopt HIST_IGNORE_SPACE
setopt NONOMATCH
setopt PROMPT_SUBST

autoload -U colors && colors
autoload edit-command-line
zle -N edit-command-line

bindkey -M vicmd v edit-command-line

uid=$(id | cut -d= -f2 | cut -d\( -f1)

if [ $uid -eq 0 ]; then
   pscolor=red
else
   pscolor=green
fi

function gb() {
   git status -sb 2>/dev/null | sed -rn '1s/^## (.*)(\.\.\..*)?$/\1/p'
}

function gb_prompt {
   branch=$(gb)
   test -n "$branch" && echo "%{%F{yellow}%} ($branch)"
}

export PS1='%{%B%F{$pscolor}%}%n@%m:%.$(gb_prompt)%{%F{$pscolor}%}>%{%b%f%}%  '
export LSCOLORS=Exfxcxdxbxegedabagacad
export LESS_TERMCAP_mb=$(tput bold; tput setaf 7) # white
export LESS_TERMCAP_md=$(tput bold; tput setaf 1) # red
export LESS_TERMCAP_me=$(tput sgr0)
export LESS_TERMCAP_so=$(tput bold; tput setaf 3; tput setab 4) # yellow on blue
export LESS_TERMCAP_se=$(tput rmso; tput sgr0)
export LESS_TERMCAP_us=$(tput smul; tput bold; tput setaf 2) # green
export LESS_TERMCAP_ue=$(tput rmul; tput sgr0)

if [ -f ~/.env ]; then
   # source environment specific to this machine
   . ~/.env
fi

if [ -f ~/.aliases ]; then
   . ~/.aliases
fi

if [ -f ~/.aliases_loc ]; then
   # source aliases specific to this machine
   . ~/.aliases_loc
fi

if [ -f ~/.funcs ]; then
   . ~/.funcs
fi

if [ -f ~/.funcs_loc ]; then
   # source functions specific to this machine
   . ~/.funcs_loc
fi


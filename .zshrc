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
setopt NONOMATCH
setopt HIST_IGNORE_SPACE

autoload -U colors && colors

uid=$(id | cut -d= -f2 | cut -d\( -f1)

if [ $uid -eq 0 ]; then
   pscolor=red
else
   pscolor=green
fi

export PS1="%{%B%F{$pscolor}%}%n@%m:%.>%{%b%f%}%  "
export LSCOLORS=Exfxcxdxbxegedabagacad

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


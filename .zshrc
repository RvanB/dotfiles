# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

export ZSH="/home/rvan/.oh-my-zsh"
export SHELL=/bin/zsh
#export TERM=screen-256color
export PATH="/home/rvan/miniconda3/bin:$PATH"

DISABLE_UNTRACKED_FILES_DIRTY="true"

KEYTIMEOUT=1
skip_global_compinit=1
ZSH_THEME="clean"

plugins=(
  git
  zsh-syntax-highlighting
  zsh-completions
  colored-man-pages
)

source $ZSH/oh-my-zsh.sh

POWERLEVEL9K_VCS_MODIFIED_BACKGROUND='red' # change git modified color to red rather than orange

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm

# Change ls colors
LS_COLORS="ow=01;36;40" && export LS_COLORS
LSCOLORS="exgxxxxxbxxxxxxxxxxxxx"; export LSCOLORS

#make cd use the ls colors
zstyle ':completion:*' list-coloors "${(@s.:.)LS_COLORS}"
autoload -Uz compinit
compinit
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

alias hg='history | grep'

alias vim="nvim"

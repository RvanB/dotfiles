# Raiden van Bronkhorst
# zsh configuration

########## ZSH CONFIGURATIONS ##########
export PATH="$HOME/.local/bin:$PATH"

# AWS
# export PATH="$HOME/aws/:$HOME/aws/aws-cli/:$PATH"

# Cargo binaries
export PATH="$HOME/.cargo/bin/:$PATH"

# # pyenv
# export PYENV_ROOT="$HOME/.pyenv"
# command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
# eval "$(pyenv init -)"


########## COMPLETIONS ##########

# fpath=(~/.zsh/completion $fpath)

# Partial completion
# zstyle ':completion:*' completer _complete

# Case insensitivity
# zstyle ':completion:*' matcher-list '' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' '+l:|=* r:|=*'

# autoload -Uz compinit && compinit -i

# Suggestions
# source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
# ZSH_AUTOSUGGEST_STRATEGY=completion

########## THEMING ##########

# Load version control information
# autoload -Uz vcs_info
#zstyle ':vcs_info:*' enable git svn
# precmd() { vcs_info }

# Enable colors
autoload -U colors && colors

# zstyle ':vcs_info:git*' formats "%b"

setopt PROMPT_SUBST

function exit_code_prompt {
  local EXIT_CODE=$?
  local COLOR="%{$fg[green]%}"
  if [[ $EXIT_CODE -ne 0 ]]; then
      echo "%{$fg[red]%}$EXIT_CODE%{$reset_color%}"
  fi
}

function git_prompt() {
  local STATUS=$(git status --short 2> /dev/null)
  if [[ -n $STATUS ]]; then
    echo "%{$fg[red]%}*%{$reset_color%} "
  fi
}

PROMPT=$'%f%T %K{blue}%F{white}%n@%m%f%k %{$fg[magenta]%}${PWD/#$HOME/~}
%{$fg[blue]%}$ %{$reset_color%}'
RPROMPT=$'$(exit_code_prompt)'
########## ALIASES ###########

alias hg="history | grep"
alias v="nvim"
alias vim="nvim"
# alias tmux="TERM=xterm-256color tmux"
alias t="tmux"
alias ls="exa --group-directories-first"

# Configuration files
alias cfemacs='vim ~/dotfiles/emacs/config.org'
alias cfvim="vim ~/dotfiles/nvim/.config/nvim/init.vim"
alias cfzsh='vim ~/dotfiles/zsh/.zshrc'
alias cftmux='vim ~/dotfiles/tmux/.tmux.conf'

if [[ "$TERM" == "dumb" ]]
then
    unsetopt zle
    unsetopt prompt_cr
    unsetopt prompt_subst
    PS1='$ '
fi

########## PROGRAM SETUP ##########

# echo "Loading nvm..."
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && source "$NVM_DIR/nvm.sh" # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
# echo "Now using node $(nvm current)"

# FZF
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

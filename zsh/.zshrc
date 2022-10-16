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
    COLOR="%{$fg[red]%}"
  fi
  echo "$COLOR$EXIT_CODE%{$reset_color%}"
}

function git_prompt() {
  local STATUS=$(git status --short 2> /dev/null)
  if [[ -n $STATUS ]]; then
    echo "%{$fg[red]%}*%{$reset_color%} "
  fi
}

PROMPT=$'%n@%m $(exit_code_prompt) %{$fg[magenta]%}${PWD/#$HOME/~}%{$reset_color%} %{$fg[cyan]%}$ %{$reset_color%}'

########## ALIASES ###########

alias hg="history | grep"
alias v="nvim"
alias vim="nvim"
# alias tmux="TERM=xterm-256color tmux"
alias t="tmux"
alias ls="exa --group-directories-first -s ext"
alias l='ls'
alias ll='ls -l'
alias la='ls -la'
alias ca='conda activate'
alias cda='conda deactivate'
alias tt='tt -notheme -showwpm -blockcursor'

# Configuration files
alias cfemacs='vim ~/dotfiles/emacs/.emacs'
alias cfvim="vim ~/dotfiles/nvim/.config/nvim/init.vim"
alias cfkitty='vim ~/dotfiles/kitty/.config/kitty/kitty.conf'
alias cfalacritty='vim ~/dotfiles/alacritty/.config/alacritty/alacritty.yml'
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

# Tere
tere() {
    local result=$(command tere --autocd-timeout off "$@")
    [ -n "$result" ] && cd -- "$result"
}

# Keybindings
bindkey -s "^F" "tere^M"
bindkey -s "^K" "ls^M"

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('$HOME/miniconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "$HOME/miniconda3/etc/profile.d/conda.sh" ]; then
        . "$HOME/miniconda3/etc/profile.d/conda.sh"
    else
        export PATH="$HOME/miniconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<


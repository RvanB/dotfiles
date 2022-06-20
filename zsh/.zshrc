# Raiden van Bronkhorst
# zsh configuration

########## ZSH CONFIGURATIONS ##########
export PATH="$HOME/.local/bin:$PATH"

# Cargo binaries
export PATH="$HOME/.cargo/bin/:$PATH"

# virtualenvwrapper
export WORKON_HOME="$HOME/envs"
export VIRTUALENVWRAPPER_PYTHON=$(which python3)
source $(which virtualenvwrapper.sh)

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

PROMPT=$'$(exit_code_prompt) %{$fg[magenta]%}${PWD/#$HOME/~}%{$reset_color%} %{$fg[cyan]%}$ %{$reset_color%}'

########## ALIASES ##########
#
alias hg="history | grep"
# alias v="nvim"
# alias vim="nvim"
# alias tmux="TERM=xterm-256color tmux"
alias t="tmux"
alias l='ls'
alias ll='ls -l'
alias la='ls -la'
alias emacs='emacs -nw'

# Configuration files
alias cfemacs='emacs ~/dotfiles/emacs/.emacs'
alias cfkitty='emacs ~/dotfiles/kitty/.config/kitty/kitty.conf'
alias cfalacritty='emacs ~/dotfiles/alacritty/.config/alacritty/alacritty.yml'
alias cfzsh='emacs ~/dotfiles/zsh/.zshrc'
alias cftmux='emacs ~/dotfiles/tmux/.tmux.conf'

########## PROGRAM SETUP ##########

# echo "Loading nvm..."
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && source "$NVM_DIR/nvm.sh" --no-use # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
# echo "Now using node $(nvm current)"

# FZF
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

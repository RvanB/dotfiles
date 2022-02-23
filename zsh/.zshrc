# Raiden van Bronkhorst
# zsh configuration

########## ZSH CONFIGURATIONS ##########
# Auto cd into directories
setopt auto_cd

export PATH="$HOME/.local/bin:$PATH"

# Cargo binaries
export PATH="$HOME/.cargo/bin/:$PATH"

########## COMPLETIONS ##########

fpath=(~/.zsh/completion $fpath)

# Partial completion
zstyle ':completion:*' completer _complete

# Case insensitivity
zstyle ':completion:*' matcher-list '' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' '+l:|=* r:|=*'

autoload -Uz compinit && compinit -i

# Suggestions
source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
ZSH_AUTOSUGGEST_STRATEGY=completion

########## THEMING ##########

# Load version control information
autoload -Uz vcs_info
#zstyle ':vcs_info:*' enable git svn
precmd() { vcs_info }

# Enable colors
autoload -U colors && colors

zstyle ':vcs_info:git*' formats "%b"

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
    echo "*"
  fi
}

PROMPT='$(exit_code_prompt) %{$fg[blue]%}${PWD/#$HOME/~}%{$reset_color%} $ '
RPROMPT='$(git_prompt) ${vcs_info_msg_0_}'

########## ALIASES ##########
#
alias hg='history | grep'
alias v="nvim"
alias vim="nvim"
alias tmux="TERM=xterm-256color tmux"
alias ls='exa -ah --color=always'

# Configuration files
alias cfkitty='vim ~/.config/kitty/kitty.conf'
alias cfzsh='vim ~/.zshrc'
alias cfvim='vim ~/.config/nvim/init.vim'
alias cftmux='vim ~/.tmux.conf'

########## PROGRAM SETUP ##########

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
# conda activate

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && source "$NVM_DIR/nvm.sh" --no-use # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# Disable Homebrew auto update
export HOMEBREW_NO_AUTO_UPDATE=1

# FZF
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

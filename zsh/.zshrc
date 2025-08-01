# Raiden van Bronkhorst
# zsh configuration

########## ZSH CONFIGURATIONS ##########
export PATH="$HOME/.local/bin:$PATH"

# Cargo binaries
export PATH="$HOME/.cargo/bin/:$PATH"

# My own programs
export PATH="$HOME/bin:$PATH"

# CPAN
eval "$(perl -I$HOME/perl5/lib/perl5 -Mlocal::lib=$HOME/perl5)"
export PATH="$HOME/perl5/bin:$PATH"

# OpenCode
export PATH=/Users/rvan/.opencode/bin:$PATH

export HISTFILE=~/.zsh_history
HISTSIZE=100000
SAVEHIST=$HISTSIZE
setopt appendhistory
# # pyenv
# export PYENV_ROOT="$HOME/.pyenv"
# command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
# eval "$(pyenv init -)"

########## SECRETS ##########
# Use Copilot chat LLMs for Aider (and aidermacs)

# export OPENAI_API_BASE="https://api.githubcopilot.com"
# export OPENAI_API_KEY=$(pass show github.com/copilot/token > 2>&1)

export CODEIUM_API_KEY=$(pass show windsurf.com/api 2> /dev/null)

# Set environment variables for Claude on Bedrock:
$(pass show aws.com/bedrock/inference-profile 2> /dev/null)

########## COMPLETIONS ##########

# fpath=(~/.zsh/completion $fpath)

# Partial completion
zstyle ':completion:*' completer _complete

# Case insensitivity
zstyle ':completion:*' matcher-list '' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' '+l:|=* r:|=*'

# autoload -Uz compinit && compinit -i

# Suggestions
source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
ZSH_AUTOSUGGEST_STRATEGY=completion

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
PROMPT='$(exit_code_prompt) %f%B%{$fg[250]%}%T%b %{$fg[245]%}%n@%m%f%k %B%{$fg[250]%}${PWD/#$HOME/~}%b%f $(git_prompt)> %{$reset_color%}'

########## ALIASES AND UTILITY FUNCTIONS ###########

alias tmux="TERM=xterm-256color tmux"

# if [[ "$TERM" == "dumb" ]]
# then
    # unsetopt zle
    # unsetopt prompt_cr
    # unsetopt prompt_subst
    # PS1='$ '
# fi

export SESSION_SCRIPT_DIR=~/.cdl-ssm-util

# Start a session on an instance by name. Searches within every SSO profile defined in
# ~/.aws/config for the instance name (with a cache for speed).
#
# Usage e.g. `session pub-aws2-ops`
session() {
    "$SESSION_SCRIPT_DIR/session.py" "$@"
}

# Auto-completion for 'session' (thanks chatgpt)
#
# Usage e.g `session pub-TAB`
autoload -Uz compinit
compinit
_session() {
    local words
    words=($(cut -f2 ~/.aws/cdl-inst-cache/*))
    _describe 'hosts' words
}
compdef _session session

########## PROGRAM SETUP ##########

# echo "Loading nvm..."
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && source "$NVM_DIR/nvm.sh" # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# FZF
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
export FZF_DEFAULT_COMMAND='find .'

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"

[ -n "$EAT_SHELL_INTEGRATION_DIR" ] && \
  source "$EAT_SHELL_INTEGRATION_DIR/zsh"


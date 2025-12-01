# Raiden van Bronkhorst
# zsh configuration

########## ZSH CONFIGURATIONS ##########
export PATH="$HOME/.local/bin:$PATH"

# Add Emacs to path on MacOS 
if [[ "$OSTYPE" == "darwin"* ]]; then
    # Expects Emacs.app from jimeh/emacs-builds
    export PATH="/Applications/Emacs.app/Contents/MacOS/bin:$PATH"
fi

# Deno (lspx)
export PATH="/Users/rvanbron/.deno/bin:$PATH"
# Cargo binaries
export PATH="$HOME/.cargo/bin/:$PATH"

export PKG_CONFIG_PATH=/opt/local/lib/opencv4/pkgconfig:$PKG_CONFIG_PATH

# My own programs
export PATH="$HOME/bin:$PATH"

# CPAN
if [[ -d "$HOME/perl5/lib/perl5/local" ]]; then
    eval "$(perl -I$HOME/perl5/lib/perl5 -Mlocal::lib=$HOME/perl5)"
    export PATH="$HOME/perl5/bin:$PATH"
fi

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
# Set environment variables for Claude on Bedrock:
# $(pass show aws.com/bedrock/inference-profile 2> /dev/null)

ANTHROPIC_DEFAULT_SONNET_MODEL=$(pass show aws.com/bedrock/sonnet 2>/dev/null)
[[ -n "$ANTHROPIC_DEFAULT_SONNET_MODEL" ]] && export ANTHROPIC_DEFAULT_SONNET_MODEL

ANTHROPIC_DEFAULT_HAIKU_MODEL=$(pass show aws.com/bedrock/haiku 2>/dev/null)
[[ -n "$ANTHROPIC_DEFAULT_HAIKU_MODEL" ]] && export ANTHROPIC_DEFAULT_HAIKU_MODEL

[[ -n "$ANTHROPIC_DEFAULT_HAIKU_MODEL" ]] && export ANTHROPIC_MODEL=$ANTHROPIC_DEFAULT_HAIKU_MODEL
[[ -n "$ANTHROPIC_DEFAULT_HAIKU_MODEL" ]] && export CLAUDE_CODE_SUBAGENT_MODEL=$ANTHROPIC_DEFAULT_HAIKU_MODEL

# export OPENAI_API_BASE="https://api.githubcopilot.com"
# export OPENAI_API_KEY=$(pass show github.com/copilot/token > 2>&1)

# export CODEIUM_API_KEY=$(pass show windsurf.com/api 2> /dev/null)

########## COMPLETIONS ##########

# fpath=(~/.zsh/completion $fpath)

# Partial completion
zstyle ':completion:*' completer _complete

# Case insensitivity
zstyle ':completion:*' matcher-list '' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' '+l:|=* r:|=*'

# autoload -Uz compinit && compinit -i

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

PROMPT='┌─ $(exit_code_prompt) %f%B%T%b %{$fg[blue]%}%n@%m%f %B${PWD/#$HOME/~}%b%f $(git_prompt)
└─> %{$reset_color%}'

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

# Autocomplete for session
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

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
export FZF_DEFAULT_COMMAND='find .'

# Eat
[ -n "$EAT_SHELL_INTEGRATION_DIR" ] && source "$EAT_SHELL_INTEGRATION_DIR/zsh"

# Krita AI integration
if [[ -s "$HOME/Library/Application Support/krita/ai_diffusion/server/uv/env" ]]; then
  . "$HOME/Library/Application Support/krita/ai_diffusion/server/uv/env"
fi

# sdkman
export SDKMAN_DIR="$HOME/.sdkman"
source "$HOME/.sdkman/bin/sdkman-init.sh"

# Added by LM Studio CLI (lms)
export PATH="$PATH:/Users/rvanbron/.lmstudio/bin"
# End of LM Studio CLI section


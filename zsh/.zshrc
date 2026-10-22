# Raiden van Bronkhorst
# zsh configuration

########## PATH ##########

typeset -U path PATH

# Prioritize homebrew over MacOS packages
[[ -d "/opt/homebrew/bin" ]] && path=("/opt/homebrew/bin" $path)
[[ -d "/opt/homebrew/opt/curl/bin" ]] && path=("/opt/homebrew/opt/curl/bin" $path)

[[ -d "$HOME/.local/bin" ]] && path=("$HOME/.local/bin" $path)

# Expects Emacs.app from jimeh/emacs-builds
[[ -d "/Applications/Emacs.app/Contents/MacOS/bin" ]] && \
  path=("/Applications/Emacs.app/Contents/MacOS/bin" $path)

# Deno (lspx)
[[ -d "$HOME/.deno/bin" ]] && path=("$HOME/.deno/bin" $path)

# Built-in Python
[[ -d "/$HOME/Library/Python/3.9/bin" ]] && path=("$HOME/Library/Python/3.9/bin" $path)

# Cargo binaries
[[ -d "$HOME/.cargo/bin" ]] && path=("$HOME/.cargo/bin" $path)

# My own programs
[[ -d "$HOME/bin" ]] && path=("$HOME/bin" $path)

# Go
[[ -d "$HOME/go/bin" ]] && path=("$HOME/go/bin" $path)

# OpenCode
[[ -d "$HOME/.opencode/bin" ]] && path=("$HOME/.opencode/bin" $path)

# LM Studio CLI
[[ -d "$HOME/.lmstudio/bin" ]] && path=("$HOME/.lmstudio/bin" $path)

# CPAN
if [[ -d "$HOME/perl5/lib/perl5/local" ]] && command -v perl >/dev/null 2>&1; then
    eval "$(perl -I"$HOME/perl5/lib/perl5" -Mlocal::lib="$HOME/perl5")"
    [[ -d "$HOME/perl5/bin" ]] && path=("$HOME/perl5/bin" $path)
fi

# pkg-config for OpenCV via MacPorts
[[ -d /opt/local/lib/opencv4/pkgconfig ]] && \
  export PKG_CONFIG_PATH="/opt/local/lib/opencv4/pkgconfig${PKG_CONFIG_PATH:+:$PKG_CONFIG_PATH}"

# Shells started only to run a command, like `zsh -i -c ...`, do not need
# prompts, completions, keybindings, secrets, nvm, fzf, or SDKMAN.
[[ -n "$ZSH_EXECUTION_STRING" ]] && return

########## zsh settings ##########

export HISTFILE="$HOME/.zsh_history"
HISTSIZE=100000
SAVEHIST=$HISTSIZE
setopt appendhistory
setopt promptsubst
setopt INC_APPEND_HISTORY
setopt SHARE_HISTORY
setopt autopushd
setopt pushd_to_home

########## COMPLETIONS ##########

zmodload zsh/complist

autoload -Uz compinit
compinit

# fpath=(~/.zsh/completion $fpath)

# Partial completion
zstyle ':completion:*' completer _complete

# Case insensitivity
zstyle ':completion:*' matcher-list '' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' '+l:|=* r:|=*'

bindkey $'\e[Z' reverse-menu-complete

source "$HOME/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh"


########## THEMING ##########

autoload -U colors && colors
autoload -U add-zsh-hook

source "$HOME/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"

# add-zsh-hook chpwd auto_activate_uv
# auto_activate_uv

rvb_prompt() {
    local exit_code=$?
    local directory="${PWD/#$HOME/~}"
    local max_directory_length=40
    local prompt_char='>'
    local user_host_color=10
    local remote_prefix=

    if (( EUID == 0 )); then
        prompt_char='#'
        user_host_color=9
    elif [[ -n "$SSH_CONNECTION" || -n "$SSH_TTY" ]]; then
        user_host_color=3
        remote_prefix='[ssh] '
    fi

    if [[ -n "$PROFILE" ]]; then
        local profile_string="%F{6}($PROFILE)%f "
    else
        local profile_string=""
    fi
    
    print -nr -- \
        "%F{7}┌─$(printf '%3d' "$exit_code")%f ${profile_string}%F{15}%D{%H:%M}%f %B%F{6}${directory}%f%b"$'\n'"%F{7}└─${prompt_char}%f "
}

PROMPT='$(rvb_prompt)'

########## ALIASES AND UTILITY FUNCTIONS ###########

alias status="git status"
alias branch="git branch --show-current"
alias push="git push"
alias pull="git pull"
alias commit="git commit"

function htprep() {
  # If PROFILE is set, run "session d2d-zephir-htprep-$PROFILE"
  if [[ -n "$PROFILE" ]]; then
      session "d2d-zephir-htprep-$PROFILE"
  else
    echo "PROFILE is not set. Please set PROFILE to the desired environment."
  fi
}

function htmm() {
  # If PROFILE is set, run "session d2d-zephir-htmm-$PROFILE"
  if [[ -n "$PROFILE" ]]; then
      session "d2d-zephir-htmm-$PROFILE"
  else
    echo "PROFILE is not set. Please set PROFILE to the desired environment."
  fi
}

function agua() {
  # If PROFILE is set, run "session d2d-zephir-agua-$PROFILE"
  if [[ -n "$PROFILE" ]]; then
      session "d2d-agua-agua-$PROFILE"
  else
    echo "PROFILE is not set. Please set PROFILE to the desired environment."
  fi
}

function papr() {
  # If PROFILE is set, run "session d2d-zephir-papr-$PROFILE"
  if [[ -n "$PROFILE" ]]; then
      session "d2d-agua-papr-$PROFILE"
  else
    echo "PROFILE is not set. Please set PROFILE to the desired environment."
  fi
}

function p() {
  local profile_name="$1"

  # If profile is not provided, unset AWS_PROFILE and PROFILE
  if [[ -z "$profile_name" ]]; then
      unset AWS_PROFILE
      unset PROFILE
      echo "Unset AWS_PROFILE and PROFILE"
      return
  fi
  
  if [[ -f "$HOME/.aws/config" ]]; then
      # Find the profile containing profile_name in ~/.aws/config and set AWS_PROFILE to it
      local profile
      profile=$(awk -v name="$profile_name" '
            BEGIN { found = 0 }
            /^\[profile / {
                if ($0 ~ name) {
                    found = 1
                    gsub(/^\[profile /, "", $0)
                    gsub(/\]$/, "", $0)
                    print $0
                    exit
                }
            }
            END { if (!found) exit 1 }
        ' "$HOME/.aws/config")
      echo "Setting AWS_PROFILE to '$profile'"
      export AWS_PROFILE="$profile"
  fi

  echo "Setting PROFILE to '$profile_name'"
  export PROFILE="$profile_name"
}

if command -v emacs >/dev/null 2>&1; then
    export EDITOR="emacsclient -n"
elif command -v vim >/dev/null 2>&1; then
    export EDITOR="vim"
elif command -v nvim >/dev/null 2>&1; then
    export EDITOR="nvim"
elif command -v vi >/dev/null 2>&1; then
    export EDITOR="vi"
fi

command -v tmux >/dev/null 2>&1 && alias tmux="TERM=xterm-256color tmux"

[[ -n "$EDITOR" ]] && alias cfzsh="$EDITOR ~/.zshrc"
[[ -n "$EDITOR" ]] && alias cfemacs="$EDITOR ~/.emacs.d/init.el"
[[ -n "$EDITOR" ]] && alias cfghostty="$EDITOR ~/.config/ghostty/config"
[[ -n "$EDITOR" ]] && alias cfnvim="$EDITOR ~/.config/nvim/init.vim"
[[ -n "$EDITOR" ]] && alias e="$EDITOR"

# if [[ "$TERM" == "dumb" ]]; then
#     unsetopt zle
#     unsetopt prompt_cr
#     unsetopt promptsubst
#     PS1='$ '
# fi

if [[ -d "$HOME/.cdl-ssm-util" ]]; then
    export SESSION_SCRIPT_DIR="$HOME/.cdl-ssm-util"
fi

# Start a session on an instance by name. Searches within every SSO profile defined in
# ~/.aws/config for the instance name (with a cache for speed).
#
# Usage e.g. `session pub-aws2-ops`
session() {
    if [[ -n "$SESSION_SCRIPT_DIR" ]] && [[ -x "$SESSION_SCRIPT_DIR/session.py" ]]; then
        "$SESSION_SCRIPT_DIR/session.py" "$@"
    else
        echo "session.py not found in SESSION_SCRIPT_DIR" >&2
        return 1
    fi
}

_session() {
    local -a words cache_files
    cache_files=("$HOME"/.aws/cdl-inst-cache/*(N))

    if (( ${#cache_files} )); then
        words=($(cut -f2 $cache_files 2>/dev/null))
        _describe 'hosts' words
    fi
}

compdef _session session

# Better ls
ls() {
  if (( $+commands[eza] )); then
    eza --group-directories-first -a --git "$@"
  elif (( $+commands[exa] )); then
    exa --group-directories-first -a --git "$@"
  else
    command ls "$@"
  fi
}

# Directory navigation
setopt AUTO_CD
autoload -U add-zsh-hook

dir-context() {
  [[ -o interactive ]] || return
  clear
}

# add-zsh-hook chpwd dir-context

if [[ -o zle ]]; then
  up-directory-widget() {
    builtin cd .. || return
    zle reset-prompt
  }
  zle -N up-directory-widget
fi

down-directory-widget() {
  local -a dirs
  dirs=(./*(/N))
}

########## PROGRAM SETUP ##########

# nvm
if [[ -d "$HOME/.nvm" ]]; then
    export NVM_DIR="$HOME/.nvm"
    [[ -s "$NVM_DIR/nvm.sh" ]] && source "$NVM_DIR/nvm.sh"
    [[ -s "$NVM_DIR/bash_completion" ]] && source "$NVM_DIR/bash_completion"
fi

# fzf
[[ -f "$HOME/.fzf.zsh" ]] && source "$HOME/.fzf.zsh"
command -v find >/dev/null 2>&1 && export FZF_DEFAULT_COMMAND='find .'

function f() {
    local dir
    dir=$(find ~/features -maxdepth 1 -type d -name "$1*" | fzf)
    [[ -n "$dir" ]] && cd "$dir"
}

# Eat
[[ -n "$EAT_SHELL_INTEGRATION_DIR" ]] && [[ -f "$EAT_SHELL_INTEGRATION_DIR/zsh" ]] && \
  source "$EAT_SHELL_INTEGRATION_DIR/zsh"

# Krita AI integration
[[ -f "$HOME/Library/Application Support/krita/ai_diffusion/server/uv/env" ]] && \
  source "$HOME/Library/Application Support/krita/ai_diffusion/server/uv/env"

# sdkman
if [[ -d "$HOME/.sdkman" ]]; then
    export SDKMAN_DIR="$HOME/.sdkman"
    [[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"
fi

# Added by LM Studio CLI (lms)
export PATH="$PATH:/Users/rvanbron/.lmstudio/bin"
# End of LM Studio CLI section

eval "$(direnv hook zsh)"

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

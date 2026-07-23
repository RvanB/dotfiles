# Raiden van Bronkhorst
# zsh configuration

########## PATH ##########

typeset -U path PATH

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

########## COMPLETIONS ##########

zmodload zsh/complist

autoload -Uz compinit
compinit

# fpath=(~/.zsh/completion $fpath)

# Partial completion
#zstyle ':completion:*' completer _complete
zstyle ':completion:*' menu select

# Case insensitivity
zstyle ':completion:*' matcher-list '' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' '+l:|=* r:|=*'

bindkey '^I' menu-select
bindkey $'\e[Z' reverse-menu-complete

source "$(brew --prefix)/share/zsh-autosuggestions/zsh-autosuggestions.zsh"


########## THEMING ##########

autoload -U colors && colors
autoload -U add-zsh-hook

exit_code_prompt() {
    local exit_code=$?
    if [[ $exit_code -ne 0 ]]; then
        echo "%{$fg[red]%}$exit_code%{$reset_color%}"
    fi
}

git_prompt() {
    local gitstatus="$(git status --short 2>/dev/null)"
    if [[ -n "$gitstatus" ]]; then
        echo "%{$fg[red]%}*%{$reset_color%} "
    fi
}

source "$(brew --prefix)/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"

# add-zsh-hook chpwd auto_activate_uv
# auto_activate_uv

venv_prompt() {
    [[ -n "$VIRTUAL_ENV" ]] && echo "(${VIRTUAL_ENV:h:t}) "
}

PROMPT='$(exit_code_prompt) $(venv_prompt)%f%B%T%b %{$fg[blue]%}%n@%m%f %B${PWD/#$HOME/~}%b%f $(git_prompt)> %{$reset_color%}'

########## ALIASES AND UTILITY FUNCTIONS ###########

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
    eza --group-directories-first --icons -a --git "$@"
  elif (( $+commands[exa] )); then
    exa --group-directories-first --icons -a --git "$@"
  else
    command ls "$@"
  fi
}

# What to show after clearing
# post-clear-list() {
#     ls
# }


# Make `clear` also show directory contents
# clear() {
#   command clear
#   post-clear-list
# }

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

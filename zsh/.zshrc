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

########## zsh settings ##########

export HISTFILE="$HOME/.zsh_history"
HISTSIZE=100000
SAVEHIST=$HISTSIZE
setopt appendhistory
setopt promptsubst

########## SECRETS ##########
# Set environment variables for Claude on Bedrock:
# $(pass show aws.com/bedrock/inference-profile 2>/dev/null)

if command -v pass >/dev/null 2>&1; then
    ANTHROPIC_DEFAULT_SONNET_MODEL="$(pass show aws.com/bedrock/sonnet 2>/dev/null)"
    [[ -n "$ANTHROPIC_DEFAULT_SONNET_MODEL" ]] && export ANTHROPIC_DEFAULT_SONNET_MODEL

    OPENAI_API_KEY="$(pass show github.com/copilot/token 2>/dev/null)"
    [[ -n "$OPENAI_API_KEY" ]] && export OPENAI_API_KEY

    CODEIUM_API_KEY="$(pass show windsurf.com/api 2>/dev/null)"
    [[ -n "$CODEIUM_API_KEY" ]] && export CODEIUM_API_KEY

    [[ -n "$OPENAI_API_KEY" ]] && export OPENAI_API_BASE="https://api.githubcopilot.com"
fi

########## COMPLETIONS ##########

# fpath=(~/.zsh/completion $fpath)

# Partial completion
zstyle ':completion:*' completer _complete

# Case insensitivity
zstyle ':completion:*' matcher-list '' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' '+l:|=* r:|=*'

########## THEMING ##########

autoload -U colors && colors
autoload -U add-zsh-hook
autoload -Uz compinit

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

auto_activate_uv() {
    local uv_python venv_dir activate_script
    local found_env=0

    # Deactivate if outside the virtual environment directory
    if [[ -n "$VIRTUAL_ENV" && $PWD != "$VIRTUAL_ENV"/* ]]; then
        if typeset -f deactivate >/dev/null; then
            deactivate
        else
            unset VIRTUAL_ENV
            hash -r
        fi
    fi

    if (( $+commands[uv] )); then
        uv_python="$(uv python find 2>/dev/null)"

        if [[ -n "$uv_python" ]]; then
            venv_dir="$(dirname "$(dirname "$uv_python")")"
            activate_script="$venv_dir/bin/activate"

            if [[ -f "$activate_script" ]]; then
                found_env=1

                if [[ "$VIRTUAL_ENV" != "$venv_dir" ]]; then
                    if [[ -n "$VIRTUAL_ENV" ]] && typeset -f deactivate >/dev/null; then
                        deactivate
                    fi
                    source "$activate_script"
                fi
            fi
        fi
    fi
}

add-zsh-hook chpwd auto_activate_uv
auto_activate_uv

PROMPT='$(exit_code_prompt) %f%B%T%b %{$fg[blue]%}%n@%m%f %B${PWD/#$HOME/~}%b%f $(git_prompt)> %{$reset_color%}'

########## ALIASES AND UTILITY FUNCTIONS ###########

if command -v vim >/dev/null 2>&1; then
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

compinit

_session() {
    local -a words cache_files
    cache_files=("$HOME"/.aws/cdl-inst-cache/*(N))

    if (( ${#cache_files} )); then
        words=($(cut -f2 $cache_files 2>/dev/null))
        _describe 'hosts' words
    fi
}

compdef _session session

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

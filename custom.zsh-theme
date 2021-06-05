
PROMPT="%(?:%{$fg_bold[green]%}0 :%{$fg_bold[red]%}1 )"
PROMPT+='%{$fg[cyan]%}%~%{$reset_color%} $(git_prompt_info)> '

ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg_bold[blue]%}on %{$fg[red]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%} "
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[red]%} dirty"
ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg[blue]%}"

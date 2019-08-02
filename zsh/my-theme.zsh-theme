
local host_name="%{$FG[011]%}"
local path_string="%{$FG[214]%}%2c"
local prompt_string="❱"
## Make prompt_string red if the previous command failed.
local return_status="%(?:%{$FG[074]%}$prompt_string:%{$fg[red]%}$prompt_string)"

PROMPT="${host_name} ${path_string} ${return_status} %{$reset_color%}"

ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg[white]%}("
ZSH_THEME_GIT_PROMPT_SUFFIX=")%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY=""
ZSH_THEME_GIT_PROMPT_CLEAN=""

git_custom_prompt() {
  # branch name (.oh-my-zsh/plugins/git/git.plugin.zsh)
  local branch=$(current_branch)
  if [ -n "$branch" ]; then
    # parse_git_dirty echoes PROMPT_DIRTY or PROMPT_CLEAN (.oh-my-zsh/lib/git.zsh)
    echo "$(parse_git_dirty) $ZSH_THEME_GIT_PROMPT_PREFIX$branch$ZSH_THEME_GIT_PROMPT_SUFFIX "
  fi
}


RPROMPT=$'$(git_custom_prompt)'

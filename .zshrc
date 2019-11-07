#   _____       _ _____             
#  / ____|     (_)  __ \            
# | (___   __ _ _| |  | |_   _  ___ 
#  \___ \ / _` | | |  | | | | |/ __|
#  ____) | (_| | | |__| | |_| | (__       Sai Pandian
# |_____/ \__,_|_|_____/ \__,_|\___|      github.com/saiduc


# =============================================================================
#                         GENERAL SETTINGS AND PATHS
# =============================================================================

# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="/Users/saipandian/.oh-my-zsh"

# Path to fzf installation
export FZF_BASE="$HOME/.fzf"

export FZF_DEFAULT_COMMAND='rg --files --hidden --glob "!.git/*"'

if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='nvim'
else
  export EDITOR='nvim'
fi

ZSH_THEME="dracula"
plugins=(zsh-syntax-highlighting)
source $ZSH/oh-my-zsh.sh

# Path to conda installation
# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/Users/saipandian/miniconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/Users/saipandian/miniconda3/etc/profile.d/conda.sh" ]; then
        . "/Users/saipandian/miniconda3/etc/profile.d/conda.sh"
    else
        export PATH="/Users/saipandian/miniconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

# =============================================================================


# =============================================================================
#                                 ALIASES
# =============================================================================

alias vim="nvim"
alias python="pythonw"
alias autopep8="/Users/saipandian/miniconda3/bin/autopep8"
alias ca="conda activate"
alias cde="conda deactivate"
alias fv='nvim $(fzf)'

# =============================================================================


# =============================================================================
#                               FUNCTIONS
# =============================================================================

# fzf cd to a directory
function fd() {
  local dir
  dir=$(find ${1:-.} -path '*/\.*' -prune \
                  -o -type d -print 2> /dev/null | fzf +m) &&
  cd "$dir"
}

# fzf cd to a directory including hidden
function fda() {
  local dir
  dir=$(find ${1:-.} -type d 2> /dev/null | fzf +m) && cd "$dir"
}

# =============================================================================

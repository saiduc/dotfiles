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

ZSH_THEME="dracula"
plugins=()
source $ZSH/oh-my-zsh.sh

## Path to conda installation
## >>> conda initialize >>>
## !! Contents within this block are managed by 'conda init' !!
#__conda_setup="$('/Users/saipandian/miniconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
#if [ $? -eq 0 ]; then
#    eval "$__conda_setup"
#else
#    if [ -f "/Users/saipandian/miniconda3/etc/profile.d/conda.sh" ]; then
#        . "/Users/saipandian/miniconda3/etc/profile.d/conda.sh"
#    else
#        export PATH="/Users/saipandian/miniconda3/bin:$PATH"
#    fi
#fi
#unset __conda_setup
## <<< conda initialize <<<

# =============================================================================
#                                 ALIASES
# =============================================================================

alias ca="conda activate"
alias cde="conda deactivate"

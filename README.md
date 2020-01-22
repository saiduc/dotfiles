# dotfiles

Simple no-fuss dotfiles for easy setup of new machines.

## Setup

Should be able to clone this repo to the home directory of linux and mac machines and extract the necessary dotfiles.

### Emacs

Just move the init.el file to your ~/.emacs.d directory and the init-org.org file to ~/.emacs.d/init/ directory. Then open the init-org.org file in emacs
and run M-x org-babel-execute buffer. Then do C-u C-c C-v k to delete all the outputs of the previous command. Then save the file.
This should install all the packages you need and set up your emacs properly. You might need to separately install the pdf-tools backend.

### Neovim

Will need to use VimPlug to install and manage plugins. Install VimPlug for Neovim by: 
```
curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
```

Then use Vim command:
```
:PlugInstall
```

If you want Vim to properly work with Python the way I use it, it is recommended
to make new virtual environments (in either virtualenv, pyenv or conda) for
Python 2 and Python 3 and install pynvim and jedi in both using pip. Then,
python completion should work correctly with deoplete, and should show correct
completions for any new virtual environment you are in.

### Bash Shell

You don't really need to do anything to set up bash if you want to use it. I recommend using the zsh shell but if you really need bash, it has been set up to look pretty similar to the dracula theme in oh-my-zsh, including showing git status.

The .bash_profile file does nothing except load the .bashrc file.

If you install conda, the bash_profile may be modified to add conda to the PATH. Try to keep the changes conda makes.

## Acknowledgements

* Instructions on installing Dracula theme from https://www.draculatheme.com
* Vim ideas from Real Python website https://www.realpython.com

# dotfiles

Simple no-fuss dotfiles for easy setup of new machines.

## Setup

Should be able to clone this repo to the home directory of linux and mac machines and extract the necessary dotfiles.

### Vim/Neovim

**Note: I no longer use Vim, I have moved to Neovim. So the .vimrc file is no longer maintained. Please use Neovim and the init.vim file**

Will need to use VimPlug to install and manage plugins. Install VimPlug for Neovim by: 
```
curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
```

Then use Vim command:
```
:PlugInstall
```
For OS X users, you can set files in Finder to open with TerminalVim.app, included in this repository. This will allow you to double-click a file and open it in a new iTerm window with vim.

If you want Vim to properly work with Python the way I use it, it is recommended
to make new virtual environments (in either virtualenv, pyenv or conda) for
Python 2 and Python 3 and install pynvim and jedi in both using pip. Then,
python completion should work correctly with deoplete, and should show correct
completions for any new virtual environment you are in.

### Emacs

Just move the init.el file to your ~/.emacs.d directory and the init-org.org file to ~/.emacs.d/init/ directory. Then open the init-org.org file in emacs and save it.
This should install all the packages you need and set up your emacs properly. You might need to separately install the pdf-tools backend.

### Bash Shell

You don't really need to do anything to set up bash if you want to use it. I recommend using the zsh shell but if you really need bash, it has been set up to look pretty similar to the dracula theme in oh-my-zsh, including showing git status.

The .bash_profile file does nothing except load the .bashrc file.

If you install conda, the bash_profile may be modified to add conda to the PATH. Try to keep the changes conda makes.

## Acknowledgements

* Instructions on installing Dracula theme from https://www.draculatheme.com
* Vim ideas from Real Python website https://www.realpython.com

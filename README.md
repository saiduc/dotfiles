# dotfiles

Simple no-fuss dotfiles for easy setup of new machines, managed by GNU Stow.

## Setup

Make sure GNU Stow is installed on your system. Then, clone this repository to
your home folder and navigate to ~/dotfiles. Then, execute:
```
stow *
```

This will symlink all the files to the relevant directories. Alternatively, you
can symlink only specific application dotfiles with:
```
stow NAME
```

Additional set-up is explained below.

### Emacs

Open the init-org.org file and follow the instructions under the "Setting up New
Installation" heading.

For proper Python support, it will be necessary to make a separate virtual
environment for the Elpy dependencies and point to it in the Python section of
the config file. The location of an Anconda/Miniconda installation must also be
specified in the Python section.

### Neovim

Install VimPlug for Plugin management and execute:
```
:PlugInstall
```

It is necessary to a new virtual environments (in either virtualenv, pyenv or
conda) for Python 3 and install pynvim and jedi in both using pip.  Then, point
to it in the init.vim file and python completion should work correctly with
deoplete, and should show correct completions for any new virtual environment
you are in.

The location of libclang must also be changed for c++ completion to work.

Run :checkhealth to make sure everything works correctly.

FZF must be installed for Fuzzy File searching.

### Zsh

I have a very basic .zshrc file that requires an installation of zsh and
oh-my-zsh. FZF and ripgrep must be installed in order to use fuzzy searching.

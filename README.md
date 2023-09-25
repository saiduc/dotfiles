# dotfiles

Simple no-fuss dotfiles for easy setup of new machines, managed by GNU Stow.

## Setup

Make sure GNU Stow is installed on your system. Then, clone this repository to
your home folder and navigate to ~/dotfiles. Then, execute:
```
stow <NAME>
```
where <NAME> is the name of the folder corresponding to the dotfiles you require.
For example, for emacs:
```
stow emacs
```

## Emacs
I compile emacs myself from scratch. I use the following options when doing so:
```
--with-pgtk
--with-json
--with-native-compilation=aot 
--with-modules 
--with-xwidgets 
--with-tree-sitter 
--without-mailutils 
--without-pop 
--with-rsvg 
--with-png 
--with-tiff 
--with-jpeg
```

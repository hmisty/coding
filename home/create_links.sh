#!/bin/sh

ln -s ~/coding/home/bash_aliases ~/.bash_aliases
ln -s ~/coding/home/gitconfig ~/.gitconfig
ln -s ~/coding/home/screenrc ~/.screenrc

ln -s ~/coding/home/vimrc ~/.vimrc
mkdir -p ~/.vim/autoload ~/.vim/bundle
ln -s ~/coding/home/vim/vim-pathogen/autoload/pathogen.vim ~/.vim/autoload/pathogen.vim
ln -s ~/coding/home/vim/vim-sensible ~/.vim/bundle/vim-sensible
mkdir -p ~/.local/share/vim/undo ~/.local/share/vim/backup ~/.local/share/vim/swap
#ln -s ~/coding/home/vim/VimClojure ~/.vim/bundle/VimClojure
ln -s ~/coding/home/vim/tabular ~/.vim/bundle/tabular

## replace foreplay with fireplace
#ln -s ~/coding/home/vim/vim-foreplay ~/.vim/bundle/vim-foreplay
ln -s ~/coding/home/vim/vim-fireplace ~/.vim/bundle/vim-fireplace

## donot link classpath for using nrepl always. speed up the vim start speed
#ln -s ~/coding/home/vim/vim-classpath ~/.vim/bundle/vim-classpath

ln -s ~/coding/home/vim/vim-clojure-static ~/.vim/bundle/vim-clojure-static
ln -s ~/coding/home/vim/rainbow_parentheses.vim ~/.vim/bundle/rainbow_parentheses.vim

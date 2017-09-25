#!/bin/sh

ln -s ~/coding/home/bash_aliases ~/.bash_aliases
ln -s ~/coding/home/gitconfig ~/.gitconfig
ln -s ~/coding/home/screenrc ~/.screenrc

ln -s ~/coding/home/tmux.conf ~/.tmux.conf

ln -s ~/coding/home/vimrc ~/.vimrc
mkdir -p ~/.vim/autoload ~/.vim/bundle
git clone https://github.com/gmarik/vundle.git ~/.vim/bundle/vundle
vim +BundleInstall +qall


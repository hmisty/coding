#!/bin/sh

ln -s ~/coding/home/bash_aliases ~/.bash_aliases
ln -s ~/coding/home/gitconfig ~/.gitconfig
ln -s ~/coding/home/screenrc ~/.screenrc

if [[ `tty` =~ 'pts' ]]; then # 登陆终端是/dev/pts*，表示是远程登陆，而不是本地终端/dev/tty*
	ln -s ~/coding/home/tmux.ops.conf ~/.tmux.conf
else
	ln -s ~/coding/home/tmux.dev.conf ~/.tmux.conf
fi

ln -s ~/coding/home/vimrc ~/.vimrc
mkdir -p ~/.vim/autoload ~/.vim/bundle
git clone https://github.com/gmarik/vundle.git ~/.vim/bundle/vundle
vim +BundleInstall +qall


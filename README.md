# CODING HACKS

Coding Hacks哲学：简单，优雅，快，少 

主要内容：coding相关的极速上手教程或快速解决方案（类似cookbook的知识库），以及一些prototype和proof-of-concept代码

## 目录结构

- /home: 我的工作环境配置（超快速配置新机器），如下：
	- 操作系统：MacOSX 或 Linux (Ubuntu/CentOS)
	- Shell: bash
	- 终端管理：tmux | 教程：[5分钟掌握Linux多终端管理器tmux - a terminal multiplexer on Linux](home/tmux.md)
	- 代码管理：git
	- 编辑器：vim
- /lang: 5-10分钟熟悉一种编程语言，掌握开发方法，包括：
	- /clojure | 教程：[1小时掌握clojure交互式开发方法](lang/clojure/README.md)
	- /python
	- /java
	- /c
	- /js
	- /go
	- /perl
	- /erlang
	- /lisp
	- /swift
- blockchain: technologies in blockchain domain.
- ml: technologies in machine learning incl. deep learning domain.
- bigdata: technologies in big data & database domain, such as pgsql, hadoop, etc.
- misc: other fun, including proof-of-concepts, etc.

## 学习在终端下工作 Learning how to work with terminals 



## Tips & Tricks

```
Q: MacOSX, command line, how to convert webp to jpg in batch?
A: $ for x in ls *.webp; do  ffmpeg -i $x $x.jpg; done
```

## CheatSheets

### python

virtual environments and package management

	mkdir ~/.env
	virtualenv ~/.env/XXX
	source ~/.env/XXX/bin/activate

	pip install YYY
	pip freeze > requirements.txt

	pip install -r requirements.txt
	deactivate

### nodejs

each directory is a separate environment (node\_modules folder in current dir)

	mkdir PROJECT
	npm install --save YYY

* use --save to add a dependency entry into the package.json (auto create if not exists)

### git

register submodule

	git submodule add https://github.com/tpope/vim-sensible.git home/vim/vim-sensible
	git submodule init
	git add .
	git commit -m 'register submodule vim-sensible'

branch
	
	# local create
	git checkout -b dev

	# local look up and switch
	git branch
	git branch -a
	git checkout master
	git checkout dev

	# push to remote
	git push origin dev
	# remote look up
	git remote -v
	
	# pull from remote
	git checkout -b dev
	git pull origin dev

	# local delete
	git checkout master
	git branch -d dev

	# remote delete
	git push origin :dev

checkout submodule

	git submodule init
	git submodule update

tagging

	g tag -a TAG -m DESCRIPTION
	g tag -a TAG COMMIT_id_first7 -m DESCRIPTION

	g push --tags
	g push origin TAG

	g tag -l #查看所有tag
	g show TAG #查看TAG对应的commit信息

	git tag -d TAG
	git push origin :TAG


### vim vs emacs

mode

	| Do                     | Vim     | Emacs |
	| switch to edit mode    | <ESC>   | n/a   |
	| switch to insert mode  | i, a, o | n/a   |
	| switch to command line | :, /    | n/a   |

move around

	| Do    | Vim | Emacs |
	| left  | h   | _     |
	| down  | j   | _     |
	| up    | k   | _     |
	| right | l   | _     |

repeat

	| Do                   | Vim          | Emacs |
	| forward 3 words      | 3w           | _     |
	| forward to the 3rd , | 3f,          | _     |
	| insert 3 hello       | 3ihello<ESC> | _     |
	| delete next 3 words  | d3w          | _     |

* Notice: Emacs repeating is inconsistent. For example, <ESC>d is to delete to word end (like vim dw), however, <ESC>3d is to insert 3 'd's.

modeline

	| Do             | Vim             |
	| in-file config | set modelines=1 |

tabular

	| Do          | Vim          |
	| ascii table | :Tab /|      |

vim-foreplay
	
	lein new proj
	cd proj
	lein repl

	vim src/proj/core.clj

	K		Look up docs for keyword under cursor.
	[d		Show source for keyword under cursor.
	gf		Go to the file for the namespace under the cursor.
	cp{motion}	Eval/print the code indicated by {motion}.
	cpp		Eval/print the inner-most expr at the cursor.
	cqp		Bring up a prompt for code to eval/print.
	:A		In a test file, edit the implementation, and vice
			versa.  Basically adds or removes -test from the end
			of the current namespace and searches for it in the
			class path.

the MIT license
---
Copyright (c) 2012 Evan Liu (hmisty)

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

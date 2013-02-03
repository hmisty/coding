<!-- vim:set ft=markdown: -->

CheatSheets
===
ALL CHEATSHEETS I LOVE...

Git
---

register submodule

	git submodule add https://github.com/tpope/vim-sensible.git home/vim/vim-sensible
	git submodule init
	git add .
	git commit -m 'register submodule vim-sensible'

checkout submodule

	git submodule init
	git submodule update

Vim vs Emacs
---
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

	| Do             | Vim             | Emacs |
	| in-file config | set modelines=1 | _     |

tabular

	| Do          | Vim          | Emacs |
	| ascii table | :Tab /|      | _     |



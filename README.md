<!-- vim:set ft=markdown: -->

Motto: KEEP CODING.
===

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



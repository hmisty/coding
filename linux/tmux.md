# 5分钟学会使用tmux

tmux是terminal multiplexer的缩写，它可以让你在一个终端连接下打开多个屏幕，而且当你从远程服务器断开连接时，tmux可以在后台维持所有shell，待你下次再次连接时快速回复到之前的工作状态，是linux终端工作和编码的必备神器。

支持：各种linux，mac。

## 安装 Installation

Mac OSX:

	$ sudo brew install tmux

Linux:

	$ sudo apt-get install tmux

或者

	$ sudo yum install tmux

## 启动，退出，detach和re-attach Start, quit, detach and re-attach

启动：

	$ tmux

退出：

	$ exit

	退出所有的shell

Detach（退出tmux，但是shell都还在运行）：

	Ctrl-b d （速记法：d = detach）

查看有哪些detached sessions:

	$ tmux ls		# ls = list-sessions

Re-attach:

	$ tmux a -t <session_id>   # a = attach-session
	如果只有一个session，可以不加-t参数
	$ tmux a

## 常用操作和快捷键 Frequent operations and shortcuts

查看屏幕历史：

	Ctrl-b 然后用PageUp/PageDown键上下翻屏就好啦！非常方便。
	ESC 多按几次就退出了历史查看模式，回到交互模式。

新建一个窗口：

	Ctrl-b c （速记法：c = create）

切换窗口：

	Ctrl-b <窗口数字0-9>

上下分隔窗口成两个panel：

	Ctrl-b "

左右分隔窗口：

	Ctrl-b %

在panel间切换：

	Ctrl-b o

## 更多 More

	$ man tmux

## 完 END

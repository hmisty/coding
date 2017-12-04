# 深度学习入门资料收集

注：资料收集自网络。

## TensorFlow入门

## 代码索引

#### 1 - 简介
- **Hello World** helloworld.py : TensorFlow版hello world，展示一下tensorflow的使用。
- **Basic Operations** basic\_operations.py : 展示一些TensorFlow的操作。

#### 2 - 基础的机器学习模型
- **Linear Regression** linear\_regression.py : 线性回归。
- **Logistic Regression** logistic\_regression.py : Logistic回归。
- **Nearest Neighbor** nearest\_neighbor.py : 近邻算法。
- **K-Means** kmeans.py : K-Means算法。
- **Random Forest** random\_forest.py : 随机森林算法。



### 坑

#### linear\_regression.py

issue:

```
(cms) Evans-MacBook-Pro:deeplearning evan$ python linear_regression.py
Fontconfig error: Cannot load default config file
Traceback (most recent call last):
  File "linear_regression.py", line 11, in <module>
    import matplotlib.pyplot as plt
  File "/Users/evan/.env/cms/lib/python2.7/site-packages/matplotlib/pyplot.py", line 113, in <module>
    _backend_mod, new_figure_manager, draw_if_interactive, _show = pylab_setup()
  File "/Users/evan/.env/cms/lib/python2.7/site-packages/matplotlib/backends/__init__.py", line 60, in pylab_setup
    [backend_name], 0)
  File "/Users/evan/.env/cms/lib/python2.7/site-packages/matplotlib/backends/backend_macosx.py", line 19, in <module>
    from matplotlib.backends import _macosx
RuntimeError: Python is not installed as a framework. The Mac OS X backend will not be able to function correctly if Python is not installed as a framework. See the Python docu
mentation for more information on installing Python as a framework on Mac OS X. Please either reinstall Python as a framework, or try one of the other backends. If you are usin
g (Ana)Conda please install python.app and replace the use of 'python' with 'pythonw'. See 'Working with Matplotlib on OSX' in the Matplotlib FAQ for more information.
```

solution: create a file ~/.matplotlib/matplotlibrc and add the following code: backend: TkAgg

ref: https://stackoverflow.com/questions/21784641/installation-issue-with-matplotlib-python


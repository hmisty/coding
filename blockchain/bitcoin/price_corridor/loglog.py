#!/usr/bin/env python3
import matplotlib.pyplot as plt
from matplotlib.ticker import ScalarFormatter, FuncFormatter
import datetime
import math

genesis = datetime.date(2009, 1, 3)

#--- prepare data ---
datafile = 'data/btc_history.csv'
f = open(datafile, 'r')
f.readline() # throw away the head line
xs = []
ys = []
for line in f:
    a, b = line.rstrip().split(',')
    dt = datetime.datetime.strptime(a, '%Y/%m/%d')
    x = (dt.date() - genesis).days
    y = float(b)
    xs.append(x)
    ys.append(y)

xs.reverse()
ys.reverse()

#--- prepare figure ---

min_x = 1000
max_x = 6000
min_y = 1
max_y = 1000000

fig, ax = plt.subplots(figsize=(16,9))
ax.axis([min_x, max_x, min_y, max_y])

ax.set_yscale('log')
formatter_y = ScalarFormatter()
formatter_y.set_scientific(False)
ax.get_yaxis().set_major_formatter(formatter_y)

ticks_x = []

for i in range(min_x, max_x):
    date = genesis + datetime.timedelta(i)
    if date.month == date.day == 1:
        ticks_x.append(i)

ax.set_xscale('log')
ax.set_xticks(ticks_x)

def years(day, pos=None):
    date = genesis + datetime.timedelta(int(day))
    return date.strftime('%Y')

formatter_x = FuncFormatter(years)
ax.get_xaxis().set_major_formatter(formatter_x)

ax.grid(True, linestyle='dotted')
ax.grid(True, which='minor', axis='y', linestyle='dotted')

ax.tick_params(axis='x', which='major', rotation=90)
ax.tick_params(axis='x', which='minor', bottom=False, top=False, labelbottom=False)
#plt.xticks(rotation=90)

#regression: till 2019/9/3
end = datetime.date(2019, 9, 3)
n = (end - genesis).days
i = xs.index(n)
plt.plot(xs[:i], ys[:i])
plt.plot(xs[i:], ys[i:], 'r') #using red

#--- regression ---
fit = lambda x: 10**(-17.01593313+5.84509376*math.log10(x))
support = lambda x: 10**(-17.45+5.84509376*math.log10(x))
top = lambda x: 10**(-13.35+5.02927337*math.log10(x))
plt.plot([min_x, max_x], [fit(min_x), fit(max_x)])
plt.plot([min_x, max_x], [support(min_x), support(max_x)])
plt.plot([min_x, max_x], [top(min_x), top(max_x)])

plt.show()

#plt.savefig("out.png")

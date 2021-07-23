#!/usr/bin/env python3
import matplotlib.pyplot as plt
from matplotlib.ticker import ScalarFormatter, FuncFormatter
import datetime

fig, ax = plt.subplots(figsize=(16,9))
ax.axis([500, 10000, 1, 1000000])

ax.set_yscale('log')
formatter_y = ScalarFormatter()
formatter_y.set_scientific(False)
ax.get_yaxis().set_major_formatter(formatter_y)

genesis = datetime.date(2009, 1, 3)
ticks_x = []

for i in range(500, 10000):
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

plt.xticks(rotation=90)
plt.show()

#plt.savefig("out.png")

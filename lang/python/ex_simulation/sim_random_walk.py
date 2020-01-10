#!/usr/bin/env python3
import random
import matplotlib.pyplot as plt

CHANGES = [-1, 0, 1]

deals = []
price = 10
for i in range(10000):
    #price += random.choice(CHANGES)
    #price = price if price > 0 else 0
    price *= 1 + 0.05 * random.choice(CHANGES)
    deals.append(price)

plt.plot(deals)
plt.show()

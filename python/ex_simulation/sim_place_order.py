#!/bin/env python3
import random

# max order amount
MAX_AMOUNT = 10000

# marker makers [(price: amount)]
buys = []
sells = []

# ticks
for i in range(1000):
    # round #i
    print("round %d" % i)

    # place one buy order
    buy1_price = buys[0][0] if len(buys) else 0
    price_high = buy1_price * (1 + 0.01) # up 1%
    price_low = max(0, buy1_price * (1 - 0.5)) # down 50%
    price = (1 - random.random()) * (price_high - price_low) + price_low
    amount = (1 - random.random()) * MAX_AMOUNT

    print("place buy order at price %f with amount %f" % (price, amount))
    #place_order_buy(price, amount)

    # place one sell order
    sell1_price = sells[0][0] if len(sells) else 0
    price_high = sell1_price * (1 + 0.5) # up 50%
    price_low = max(0, sell1_price * (1 - 0.01)) # down 1%
    price = (1 - random.random()) * (price_high - price_low) + price_low
    amount = (1 - random.random()) * MAX_AMOUNT

    print("place sell order at price %f with amount %f" % (price, amount))
    #place_order_sell(price, amount)



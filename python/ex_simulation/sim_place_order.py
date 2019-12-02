#!/bin/env python3
import math
import random

# price step
PRICE_STEP = 0.01

# max order amount
MAX_AMOUNT = 10000

# marker makers [(price, amount)]
buys = []
sells = []

def place_order_buy(price, amount):
    i = 0
    for i in range(len(buys)):
        if price >= buys[i][0]:
            break

    if len(buys) > 0 and price == buys[i][0]:
        buys[i] = (buys[i][0], buys[i][1] + amount)
    elif len(buys) > 0 and price > buys[i][0]:
        buys.insert(0, (price, amount))
    else:
        buys.insert(i+1, (price, amount))

def place_order_sell(price, amount):
    i = 0
    for i in range(len(sells)):
        if price <= sells[i][0]:
            break

    if len(sells) > 0 and price == sells[i][0]:
        sells[i] = (sells[i][0], sells[i][1] + amount)
    elif len(sells) > 0 and price < sells[i][0]:
        sells.insert(i, (price, amount))
    else:
        sells.append((price, amount))


if __name__ == '__main__':
    # ticks
    for i in range(1000):
        # round #i
        print("round %d" % i)

        # place one buy order
        buy1_price = buys[0][0] if len(buys) else 0

        if random.choice([True, False]):
            price = buy1_price + PRICE_STEP # up 1 step
        else:
            price_high = buy1_price # down 0
            price_low = max(0, buy1_price - 50 * PRICE_STEP) # down 50 step
            price = random.random() * (price_high - price_low) + price_low

        amount = (1 - random.random()) * MAX_AMOUNT

        # set precision 0.01
        price = math.floor(price * 100) / 100
        amount = math.floor(amount * 100) / 100

        print("place buy order at price %f with amount %f" % (price, amount))
        place_order_buy(price, amount)

        # place one sell order
        sell1_price = sells[0][0] if len(sells) else 0

        if random.choice([True, False]):
            price = max(0, sell1_price - PRICE_STEP) # down 1 step
        else:
            price_high = sell1_price + 50 * PRICE_STEP # up 50 step
            price_low = max(0, sell1_price) # up 0
            price = random.random() * (price_high - price_low) + price_low

        amount = (1 - random.random()) * MAX_AMOUNT

        # set precision 0.01
        price = math.floor(price * 100) / 100
        amount = math.floor(amount * 100) / 100

        print("place sell order at price %f with amount %f" % (price, amount))
        place_order_sell(price, amount)


    # inspect makers
    print(buys)
    print(sells)


#!/bin/env python3
import math
import random
import matplotlib.pyplot as plt

# price step
PRICE_CHANGE = 0.001 # .1%
PRICE_START = 100
CHANGE_DIRECTION = [-1, 0, 1]

# max order amount
MAX_AMOUNT = 10000

# marker makers [(price, amount)]
buys = []
sells = []
deals = []

def place_order_buy(price, amount):
    print("match/place buy order (%f, %f)" % (price, amount))
    i = 0
    # match first
    while i < len(sells) and amount > 0:
        if price < sells[i][0]:
            break
        else: # deal
            print("matched buy order (%f, %f) with sells[%d](%f, %f)" % (price, amount, i, sells[i][0], sells[i][1]))
            if amount < sells[i][1]:
                deal_price = sells[i][0]
                deal_amount = amount
                amount -= deal_amount # == 0 remained
                # update depth list
                sells[i] = (sells[i][0], sells[i][1] - deal_amount)
                # settlement
                deals.append((deal_price, deal_amount))
            elif amount == sells[i][1]:
                deal_price = sells[i][0]
                deal_amount = amount
                amount -= deal_amount # == 0 remained
                # update depth list
                sells.pop(i)
                # settlement
                deals.append((deal_price, deal_amount))
            elif amount > sells[i][1]:
                deal_price = sells[i][0]
                deal_amount = sells[i][1]
                amount -= deal_amount # > 0 remained
                # update depth list
                sells.pop(i)
                # settlement
                deals.append((deal_price, deal_amount))

        # prepare for next loop
        i += 1

    # break if amount remains 0
    if amount == 0:
        return

    # otherwise place into depth
    for i in range(len(buys)):
        if price >= buys[i][0]:
            break

    if len(buys) == 0:
        buys.append((price, amount))
    else:
        if price == buys[i][0]:
            buys[i] = (buys[i][0], buys[i][1] + amount)
        elif price > buys[i][0]:
            buys.insert(i, (price, amount))
        else:
            buys.insert(i+1, (price, amount))

def place_order_sell(price, amount):
    print("match/place sell order (%f, %f)" % (price, amount))
    i = 0
    # match first
    while i < len(buys) and amount > 0:
        if price > buys[i][0]:
            break
        else: # deal
            print("matched sell order (%f, %f) with buys[%d](%f, %f)" % (price, amount, i, buys[i][0], buys[i][1]))
            if amount < buys[i][1]:
                deal_price = buys[i][0]
                deal_amount = amount
                amount -= deal_amount # == 0 remained
                # update depth list
                buys[i] = (buys[i][0], buys[i][1] - deal_amount)
                # settlement
                deals.append((deal_price, deal_amount))
            elif amount == buys[i][1]:
                deal_price = buys[i][0]
                deal_amount = amount
                amount -= deal_amount # == 0 remained
                # update depth list
                buys.pop(i)
                # settlement
                deals.append((deal_price, deal_amount))
            elif amount > buys[i][1]:
                deal_price = buys[i][0]
                deal_amount = buys[i][1]
                amount -= deal_amount # > 0 remained
                # update depth list
                buys.pop(i)
                # settlement
                deals.append((deal_price, deal_amount))

        # prepare for next loop
        i += 1

    # break if amount remains 0
    if amount == 0:
        return

    # then place into depth
    for i in range(len(sells)):
        if price <= sells[i][0]:
            break

    if len(sells) == 0:
        sells.append((price, amount))
    else:
        if price == sells[i][0]:
            sells[i] = (sells[i][0], sells[i][1] + amount)
        elif price < sells[i][0]:
            sells.insert(i, (price, amount))
        else:
            sells.append((price, amount))


if __name__ == '__main__':
    # init
    sell_price = PRICE_START
    buy_price = PRICE_START

    # ticks
    for i in range(1000):
        # round #i
        print("round %d" % i)

        change_direction = random.choice(CHANGE_DIRECTION)

        ################################################## 
        # prepare buy order
        buy_amount = (1 - random.random()) * MAX_AMOUNT
        buy_price *= 1 + PRICE_CHANGE * change_direction

        # set precision 0.01
        buy_price = math.floor(buy_price * 100) / 100
        buy_amount = math.floor(buy_amount)

        # place orders
        place_order_buy(buy_price, buy_amount)

        ################################################## 
        # prepare sell order
        sell_amount = (1 - random.random()) * MAX_AMOUNT
        sell_price *= 1 + PRICE_CHANGE * change_direction

        # set precision 0.01
        sell_price = math.floor(sell_price * 100) / 100
        sell_amount = math.floor(sell_amount)

        # place orders
        place_order_sell(sell_price, sell_amount)

        ################################################## 
        # inspect makers
        print(buys)
        print(sells)

        # save momentum
        momentum = change_direction

    ################################################## 
    # plot the price history
    price_history = []
    for (price, amount) in deals:
        price_history.append(price)

    plt.plot(price_history, '.-')
    plt.show()


#!/bin/env python3
import math
import random

# price step
PRICE_STEP = 0.01
PRICE_START = 10

# max order amount
MAX_AMOUNT = 10000

# marker makers [(price, amount)]
buys = [(PRICE_START - PRICE_STEP, MAX_AMOUNT)]
sells = [(PRICE_START + PRICE_STEP, MAX_AMOUNT)]
deals = []

def place_order_buy(price, amount):
    i = 0
    # match first
    while i < len(sells):
        print("matching buy order with sells[%d]" % i)
        if price < sells[i][0]:
            break
        else: # deal
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

    if len(buys) > 0 and price == buys[i][0]:
        buys[i] = (buys[i][0], buys[i][1] + amount)
    elif len(buys) > 0 and price > buys[i][0]:
        buys.insert(0, (price, amount))
    else:
        buys.insert(i+1, (price, amount))

def place_order_sell(price, amount):
    i = 0
    # match first
    while i < len(buys):
        print("matching sell order with buys[%d]" % i)
        if price > buys[i][0]:
            break
        else: # deal
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
            price = max(1, buy1_price - PRICE_STEP)
#            price_high = buy1_price # down 0
#            price_low = max(0, buy1_price - 50 * PRICE_STEP) # down 50 step
#            price = random.random() * (price_high - price_low) + price_low

        amount = (1 - random.random()) * MAX_AMOUNT

        # set precision 0.01
        price = math.floor(price * 100) / 100
        amount = math.floor(amount)

        print("place buy order at price %f with amount %d" % (price, amount))
        place_order_buy(price, amount)

        # place one sell order
        sell1_price = sells[0][0] if len(sells) else 0

        if random.choice([True, False]):
            price = max(0, sell1_price - PRICE_STEP) # down 1 step
        else:
            price = sell1_price + PRICE_STEP
#            price_high = sell1_price + 50 * PRICE_STEP # up 50 step
#            price_low = max(0, sell1_price) # up 0
#            price = random.random() * (price_high - price_low) + price_low

        amount = (1 - random.random()) * MAX_AMOUNT

        # set precision 0.01
        price = math.floor(price * 100) / 100
        amount = math.floor(amount)

        print("place sell order at price %f with amount %d" % (price, amount))
        place_order_sell(price, amount)


    # inspect makers
    print(buys)
    print(sells)
    print(deals)


#!/usr/bin/env python3

DECIMALS = 3

symbolA = input('[?]Symbol A: ')
symbolB = input('[?]Symbol B: ')
symbolAB = symbolA + '/' + symbolB


amountA = float(input('[?]Total amount of ' + symbolA + ' to be sold: '))
conn_weight = float(input('[?]Connected Weight(0-1): '))
initial_price = float(input('[?]Initial price of ' + symbolAB + ': '))

supplyA = initialA = 1000 # just for initial pricing
balanceB = initialB = round(initial_price * supplyA * conn_weight, DECIMALS)

print()

if not 0 <= conn_weight <= 1:
    print('[!]Invalid CW.')
else:
    while True:
        spot_price = round(balanceB / (supplyA * conn_weight), DECIMALS)
        print('[=]Spot price of ' + symbolAB + ': ', spot_price)
        print()

        inB = float(input('[?]Input amount of ' + symbolB + ' (0 to quit): '))

        if inB > 0:
            outA = round(supplyA * ((1 + inB/balanceB)**conn_weight - 1), DECIMALS)

            if amountA < outA:
                print('[!]Not enough liquidity for ' + symbolA, '(', amountA, '<', outA, ')')
            else:
                print('[=]Output amount of ' + symbolA + ': ', outA)
                print('[=]Effective price of ' + symbolAB + ': ', round(inB/outA, DECIMALS))

                supplyA = round(supplyA + outA, DECIMALS)
                amountA = round(amountA - outA, DECIMALS)
                balanceB = round(balanceB + inB, DECIMALS)

                print('[=]Sold ' + symbolA + ': ', round(supplyA - initialA, DECIMALS))
                print('[=]Remaining ' + symbolA + ': ', amountA)
                print('[=]Raised ' + symbolB + ': ', round(balanceB - initialB, DECIMALS))

                if amountA == 0:
                    print()
                    print('Sold out.')
                    break

            print()
        else:
            break
            

#!/usr/bin/env python3

DECIMALS = 3

## ask for some facts
symbolA = input('[?]Symbol A: ')
symbolB = input('[?]Symbol B: ')
symbolAB = symbolA + '/' + symbolB

conn_weight = float(input('[?]Connected Weight(0-1): '))
initial_price = float(input('[?]Initial price of ' + symbolAB + ': '))

max_supplyA = float(input('[?]Total amount of ' + symbolA + ' to be sold: '))
contract_balanceA = max_supplyA # put token A into the contract

## initialize
initializedA = 1000 # just for initial pricing
liquidityA = contract_balanceA - initializedA

contract_balanceB = 0 # one-way swap
initializedB = initial_price * (max_supplyA - liquidityA) * conn_weight
liquidityB = initializedB + contract_balanceB 

liquidityA = round(liquidityA, DECIMALS)
initializedB = round(initializedB, DECIMALS)
liquidityB = round(liquidityB, DECIMALS)

print()

## start
if not 0 <= conn_weight <= 1:
    print('[!]Invalid CW.')
else:
    while True:
        spot_price = liquidityB / ((max_supplyA - liquidityA) * conn_weight)
        spot_price = round(spot_price, DECIMALS)

        print('[=]Spot price of ' + symbolAB + ': ', spot_price)
        print()

        inB = float(input('[?]Input amount of ' + symbolB + ' (0 to quit): '))

        if inB > 0:
            outA = (max_supplyA - liquidityA) * ((1 + inB/liquidityB)**conn_weight - 1)
            outA = round(outA, DECIMALS)

            if contract_balanceA < outA:
                print('[!]Not enough liquidity for ' + symbolA, '(', contract_balanceA, '<', outA, ')')
            else:
                print('[=]Output amount of ' + symbolA + ': ', outA)
                print('[=]Effective price of ' + symbolAB + ': ', round(inB/outA, DECIMALS))

                # update facts
                contract_balanceA -= outA
                contract_balanceB += inB

                # do math
                liquidityA = contract_balanceA - initializedA
                liquidityB = contract_balanceB + initializedB

                # round
                contract_balanceA = round(contract_balanceA, DECIMALS)
                contract_balanceB = round(contract_balanceB, DECIMALS)
                liquidityA = round(liquidityA, DECIMALS)
                liquidityB = round(liquidityB, DECIMALS)

                print('[=]Sold ' + symbolA + ': ', round(max_supplyA - contract_balanceA, DECIMALS))
                print('[=]Remaining ' + symbolA + ': ', contract_balanceA)
                print('[=]Raised ' + symbolB + ': ', contract_balanceB)

                if contract_balanceA == 0:
                    print()
                    print('Sold out.')
                    break

            print()
        else:
            break
            

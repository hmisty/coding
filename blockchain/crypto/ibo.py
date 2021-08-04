#!/usr/bin/env python3
import pyinputplus as inp

## precision of floats
DECIMALS = 4

## ask for some facts
symbolA = inp.inputStr('[?]Symbol A: ')
symbolB = inp.inputStr('[?]Symbol B: ')
symbolAB = symbolA + '/' + symbolB

conn_weight = inp.inputNum('[?]Connected Weight(0-1): ', greaterThan=0, max=1)
initial_price = inp.inputNum('[?]Initial price of ' + symbolAB + ': ', greaterThan=0)

max_supplyA = inp.inputNum('[?]Total amount of ' + symbolA + ' to be sold: ', greaterThan=0)
contract_balanceA = max_supplyA # put token A into the contract
contract_balanceB = 0 # one-way swap

## initialize
initializedA = 1000 # just for initial pricing
initializedB = initial_price * initializedA * conn_weight # max_supplyA - liquidityA = initializedA

liquidityA = contract_balanceA - initializedA
liquidityB = contract_balanceB + initializedB

print()

## start
if not 0 <= conn_weight <= 1:
    print('[!]Invalid CW.')
else:
    while True:
        spot_price = liquidityB / ((max_supplyA - liquidityA) * conn_weight)

        print('[=]Spot price of ' + symbolAB + ': ', round(spot_price, DECIMALS))
        print()

        # branch 1: given in(B) to calculate out(A)
        inB = inp.inputNum('[?]Amount of ' + symbolB + ' to spend (0 to skip): ', min=0)

        if inB > 0:
            outA = (max_supplyA - liquidityA) * ((1 + inB/liquidityB)**conn_weight - 1)

            if contract_balanceA < outA:
                print('[!]Not enough liquidity for ' + symbolA, '(', contract_balanceA, '<', round(outA, DECIMALS), ')')
            else:
                print('[=]Output amount of ' + symbolA + ': ', round(outA, DECIMALS))
                print('[=]Effective price of ' + symbolAB + ': ', round(inB/outA, DECIMALS))

                # update facts
                contract_balanceA -= outA
                contract_balanceB += inB

                # do math
                liquidityA = contract_balanceA - initializedA
                liquidityB = contract_balanceB + initializedB

                print('[=]Sold ' + symbolA + ': ', round(max_supplyA - contract_balanceA, DECIMALS))
                print('[=]Remaining ' + symbolA + ': ', round(contract_balanceA, DECIMALS))
                print('[=]Raised ' + symbolB + ': ', round(contract_balanceB, DECIMALS))

                if round(contract_balanceA, DECIMALS) == 0:
                    print()
                    print('Sold out.')
                    break

            print()
            continue
            
        # branch 2: given out(A) to calculate in(B)
        outA = inp.inputNum('[?]Amount of ' + symbolA + ' to buy (0 to quit): ', min=0)

        if outA > 0:
            if contract_balanceA < outA:
                print('[!]Not enough liquidity for ' + symbolA, '(', contract_balanceA, '<', round(outA, DECIMALS), ')')
            else:
                inB = ((1 + outA/(max_supplyA - liquidityA))**(1/conn_weight) - 1) * liquidityB

                print('[=]Input amount of ' + symbolB + ': ', round(inB, DECIMALS))
                print('[=]Effective price of ' + symbolAB + ': ', round(inB/outA, DECIMALS))

                # update facts
                contract_balanceA -= outA
                contract_balanceB += inB

                # do math
                liquidityA = contract_balanceA - initializedA
                liquidityB = contract_balanceB + initializedB

                print('[=]Sold ' + symbolA + ': ', round(max_supplyA - contract_balanceA, DECIMALS))
                print('[=]Remaining ' + symbolA + ': ', round(contract_balanceA, DECIMALS))
                print('[=]Raised ' + symbolB + ': ', round(contract_balanceB, DECIMALS))

                if round(contract_balanceA, DECIMALS) == 0:
                    print()
                    print('Sold out.')
                    break

            print()
            continue


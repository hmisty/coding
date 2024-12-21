#!/usr/bin/python3

from decimal import Decimal
from math import sqrt

def fac(n):
    a = Decimal(1)
    for i in range(1, n+1):
        a *= i

    return a

s = Decimal(0)
for k in range(0, 10):
    d1 = fac(4*k) * (Decimal(1103) + Decimal(26390)*k) 
    d2 = fac(k)**4 * Decimal(396)**(4*k)
    s += Decimal(d1) / Decimal(d2)

s *= Decimal(sqrt(2)) * 2 / 9801

s = 1/s

print(s)

#coding:utf-8
import sys
from getpass import getpass
from bitcoin import *

myAddress = raw_input('btc address: ')
myBrainWalletPassword = getpass('brainwallet password: ')

test_msg = 'hello world'
privkey = sha256(myBrainWalletPassword)
sig = ecdsa_sign(test_msg, privkey)
ownership = ecdsa_verify(test_msg, sig, myAddress)

if ownership:
    print myAddress, 'is yours.'
else:
    print myAddress, 'is NOT yours.'


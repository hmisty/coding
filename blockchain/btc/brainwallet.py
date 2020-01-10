#coding:utf-8
import sys
from getpass import getpass
from bitcoin import *

myBrainWalletPassword = getpass('password: ')
mySecretKey = encode_privkey(sha256(myBrainWalletPassword), "wif")
myAddress = pubtoaddr(privtopub(mySecretKey))
print 'secretKey:', '\033[37;47m' + mySecretKey + '\033[0m'
print '  address:', myAddress

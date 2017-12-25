#coding:utf-8
import sys
from bitcoin import *
myBrainWalletPassword = sys.argv[1]
mySecretKey = encode_privkey(sha256(myBrainWalletPassword), "wif")
myAddress = pubtoaddr(privtopub(mySecretKey))
print 'secretKey:', mySecretKey
print '  address:', myAddress

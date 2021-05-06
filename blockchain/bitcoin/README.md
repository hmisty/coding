

## python3 ord issue

```
Traceback (most recent call last):
  File "brainwallet.py", line 24, in <module>
    ownership = ecdsa_verify(test_msg, sig, myAddress)
  File "/Library/Frameworks/Python.framework/Versions/3.7/lib/python3.7/site-packages/bitcoin/main.py", line 550, in ecdsa_verify
    return ecdsa_verify_addr(msg, sig, pub)
  File "/Library/Frameworks/Python.framework/Versions/3.7/lib/python3.7/site-packages/bitcoin/main.py", line 544, in ecdsa_verify_addr
    magic = get_version_byte(addr)
  File "/Library/Frameworks/Python.framework/Versions/3.7/lib/python3.7/site-packages/bitcoin/main.py", line 435, in get_version_byte
    return ord(data[0])
TypeError: ord() expected string of length 1, but int found
```

fix:
1. edit /Library/Frameworks/Python.framework/Versions/3.7/lib/python3.7/site-packages/bitcoin/main.py
2. change 435     return ord(data[0]) to return data[0]


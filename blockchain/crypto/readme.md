# crypto

## sha

## ecdsa

install
```
$ pip3 install ecdsa
```


## Steganography

install 7z on macosx.
```
$ brew install p7zip
```

zip with filenames hidden.
```
$ 7z a -p -mhe=on filename.7z /path/to/directory
```

hide zip file within an image.
```
$ cat image.jpg filename.7z > filename.jpg
```

the image can be viewed as usual.
and it can be directly unzipped.
```
$ 7z e -o/path/to/directory filename.jpg
```


## Bancor

pay token B (connected token = base token) to buy token A (smart token = token on sale),\
i.e. swap token B for token A,\
i.e. IBO of A, where IBO is short for Initial Bancor Offering.

assume:

CW = Connected Weight, 0-100%

pricing:

```
spot_price(A) = liquidity(B) / ((max_supply(A) - liquidity(A)) * CW)
```

swap:

```
out(A) = (max_supply(A) - liquidity(A)) * ((1 + in(B)/liquidity(B))^CW - 1)
```

where,
```
liquidity(A) = contract_balance(A) - initialized(A)
liquidity(B) = contract_balance(B) + initialized(B)
```

reference: Bancor Protocol whitepaper. Mar 18, 2018.


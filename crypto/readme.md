# crypto

## sha

## ecdsa

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

the image can be viewed as ususual.
and it can be directly unzipped.
```
$ 7z e -o/path/to/directory filename.jpg
```


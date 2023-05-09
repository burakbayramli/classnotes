# Harf Kaydirarak Sifreleme

```
def helper(message, ift):
        message = message.lower()
        sec = ""
        for c in message:
                if c in "abcdefghijklmnopqrstuvwxyz":
                        num = ord(c)
                        num += ift
                        if num > ord("z"):
     # wrap if necessary
                                num -= 26
                        elif num < ord("a"):
                                num += 26
                        sec = sec + chr(num)
                else:
                        sec = sec + c
        return secdef e(message):
        return helper(message, 3)

def d(message):
        return helper(message, -3)
```

Anahtar kelimeler: code, decode, encrypt, decrypt

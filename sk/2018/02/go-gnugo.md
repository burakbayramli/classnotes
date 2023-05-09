# Go Oyunu, GnuGo

Ubuntu'da GnuGo kurmak icin

```
apt-get install gnugo
```

Oyunu oynamak icin ornek bir komut

```
gnugo --level 10 --board 9
```

Bilgisayar 10 seviyesinde (en kolay) ve 9x9 boyutlarinda bir tahtada
oyun baslar. Hamleler c4, b1 gibi kordinatlarla verilir.

GnuGo ile iletisim kurabilecek bir Python arayuzu surada
bulunabilir. Ornek kullanim,

```
gnugo = gtp.GTPFacade("white", ["gnugo", "--mode", "gtp", "--level", "10"])
gnugo.boardsize(9)
gnugo.komi(5.5)
gnugo.clear_board()

res = gnugo.genmove(gtp.WHITE)
print 'res', res
gnugo.showboard()       
```


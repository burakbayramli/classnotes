# JAGS

Not: MCMC icin en iyi secenek Gelman'in Stan paketi.

R dilinde MCMC hesaplari yapmak icin WinBUGS'a alternatif bir paket JAGS. Once jags, sonra rjags, sonra R2jags paketlerinin kurulmasi tavsiye edilir. jags kaynaklardan kurulur, suradan.

http://www-fis.iarc.fr/~martyn/software/jags/

Bilinen ./configure, sudo make install. Geri kalan paketleri kurmak icin R komut satirindan

install.packages("rjags")
install.packages(R2jags")

Ubuntu uzerinde su hatayi gorurseniz,

libjags.so.2: cannot open shared object file: No such file or directory

O zaman sudo ldconfig ile so kutuphanelerini guncellemek yeterli.

JAGS nasil kullanilir? R icinden ornek bir kullanim Andrew Gelman'in kitabindaki radon ornegidir. Bugs icin yazilmistir ama JAGS'e gecirmek icin bugs() cagrisini jags() yapmak yeterli.  Bu sebeple BUGS'dan JAGS'e gecis cok rahat. R kodu basinda
library(R2jags)
..
radon.1 <- jags (radon.data, radon.inits, radon.parameters,
"radon.1.bug", n.chains=3, n.iter=10)


Radon Ornegi






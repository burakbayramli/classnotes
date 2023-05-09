# R, Matrix, lme4, sim ve Ubuntu


Andrew Gelman'in kitaplarindaki Bugs ve diger ornekleri isletebilmek
icin, Ubuntu apt-get komutundan gelen R degil, daha yuksek bir
versiyon gerekiyor, cunku daha alt versiyonlar Matrix kutuphanesini
kuramiyorlar. En son versiyon icin R sitesine gitmek
lazimhttp://ftp.sunet.se/pub/lang/CRAN/bin/linux/ubuntu/dizininde
kendi Ubuntu versiyonunuzu secin (bizimki
Karmic). Buradanr-base-core_2.11.1-5karmic0_i386.debadli dosyayi
indirip uzerine tiklayin. Bir GUI cikacak, oradan Install dugmesine
tiklanir. Eger daha once R kurulmus ise, tiklamadan once eski paketi
sudo apt-get remove r-base r-base-dev r-base-core .. vs ile silmek
daha temiz olur.R kurulduktan sonra R yazarak komut satirina girelim:>
install.packages("arm")> install.packages("foreign")yazarsak gereken
tum bagimli, alt kutuphaneler otomatik olarak kurulacaktir. Matrix,
lme4 paketleri bu paketlerden.. ve bu sefer R sikayet etmeyecek.ARM
Gelman hocanin kendi yazdigi kutuphanedir.






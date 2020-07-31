# Ubuntu ve Media Dosyalari

Windows'dan Ubuntu'ya gecis yaptiysaniz ve ASF, VMW tipinde eski video
dosyalari var ise, bunlari Ubuntu'da calmak icin once VLC Player,
MPlayer programlarini sudo apt-get uzerinden kurmak iyi fikir... Bazi
inatci ASF dosyalari hala direniyorlarsa, o zaman suradaki tavsiyeleri
takip edilebilir;

wget -c http://packages.medibuntu.org/pool/non-free/w/w32codecs/w32codecs_20071007-0medibuntu2_i386.deb

sudo dpkg -i w32codecs_20071007-0medibuntu2_i386.deb

Ustteki komutlardan sonra MPlayer asf dosyalarini caliyor.





# Emacs, Ubuntu, Yeni Font (Monaco)


Emacs, Ubuntu, Yeni Font (Monaco)



Suradaki baglanti Monaco isminde bir fontun Emacs icin nasil kurulacagini anlatir. Ubuntu 11 uzerinde de isledi.Font suradan alinir.sudo mv Monaco_Linux.ttf /usr/share/fonts/truetype/customsudo fc-cache -f -vecho “Emacs.font: Monaco-11″ > ~/.Xresourcesxrdb -merge ~/.XresourcesBundan sonra Monaco font isminin Emacs'te nasil kodlandigini gormek icin editoru baslatip *scratch* buffer'ina girin, ve alttaki komutu yazip, eval-buffer ile isletin.(insert (prin1-to-string (x-list-fonts "*")))Sonuc icinde "-unknown-Monaco-normal-normal...." turunde bir font tanimi gelecek. Bu ismi alip .emacs icinde(set-default-font "-unknown-Monaco-normal-...")olarak tanimlayinca Emacs'in hep kullandigi font haline getirebilirsiniz. Guzel bir font! Ozellikle Python kodunu guzel gosteriyor.





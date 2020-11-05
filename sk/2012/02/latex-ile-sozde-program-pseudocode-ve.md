# LaTeX ile Sozde Program (Pseudocode) ve Matematik Sembolleri

Eger TeX dokumani icinde sozde kod gosterilecekse cogu zaman satafatli
paketler (mesela algorithmicx, pseudocode, vs gibi) gerekli
degildir. Bu paketler ufacik bir program icin apayri baska bir
programlama dili ogrenmenizi gerektiriyor, for, while, if gibi
komutlarin o paketlerin istedigi sekilde yazilmasi gerekiyor mesela,
\IF, \WHILE gibi.. Duz kod gostermek icin kullanilan Listings paketi
kullanilabilir, eger bu kod icinde matematik sembolleri gerekiyorsa,
mathescape secenegi eklenir,
mesela\begin{lstlisting}[language=Matlab,mathescape]..\end{lstlisting}gibi,
boylece $$ isaretleri arasinda hala matematik sembolleri mumkun olur.





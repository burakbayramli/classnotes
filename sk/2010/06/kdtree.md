# kdtree


kdtree



KDTree arama amacli olarak herhangi boyutlu bir geometrik alani (ve o alandaki noktalari) bolumlere ayirabilen bir algoritmadir. Cok boyutlu noktalari bir alana dagitip / koyup, sonra elimizdeki baska bir noktanin "o noktalarin hangisine daha yakin oldugunu" sorgulamamizi saglar.KDTree'leri bir tur "cok boyutlu noktalar icin kullanilan bir veri tabani" olarak kabul edebiliriz. Bu taban icinde noktalar oyle dagitiliyor ki, "yakin nokta" arama islevi daha kolay hale geliyor.Python olarak iyi isleyen bir kod python-kdtree projesinde. Kullanim ornegi altta, ornekte kdtree'ye noktalari verip, sonra bu noktalarin icinden en yakin 2 tanesini bulmasini soyluyoruz.data = [(2,3), (5,4), (9,6), (4,7), (8,1), (7,2)]point = (8,5)tree = KDTree.construct_from_data(data)nearest = tree.query(point, t=2)print nearest





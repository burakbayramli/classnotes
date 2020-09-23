load("pca-examples.Rdata")
nyt.pca = prcomp(nyt.frame[,-1])
nyt.latent.sem = nyt.pca$rotation
plot(nyt.pca$x[,1:2],type="n")
points(nyt.pca$x[nyt.frame[,"class.labels"]=="art",1:2],
       pch="A",col="red")
points(nyt.pca$x[nyt.frame[,"class.labels"]=="music",1:2],
       pch="M",col="blue")

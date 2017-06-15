#
# Inverse Document-Frequency Weighting Code in R 
#
scale.rows <- function (x, s)  {
    return(apply(x, 2, function(x) {x * s}))
}
scale.cols <- function(x,s) {
  return(t(apply(x,1,function(x){x*s})))
}
div.by.euc.length <- function(x) {
  scale.rows(x,1/sqrt(rowSums(x^2)+1e-16))
}
idf.weight <- function(x) {
  # IDF weighting
  doc.freq <- colSums(x>0)
  doc.freq[doc.freq == 0] <- 1
  w <- log(nrow(x)/doc.freq)
  return(scale.cols(x,w))
}
nyt.frame.raw = read.csv("nytimes.csv")
nyt.frame.raw$class.labels <- NULL
nyt.frame <- idf.weight(nyt.frame.raw)
nyt.frame <- div.by.euc.length(nyt.frame)

write.csv(nyt.frame,"nytimes4.csv")

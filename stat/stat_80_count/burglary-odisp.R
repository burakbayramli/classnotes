library("MASS")

burg <- read.table ("burglary.txt", header=TRUE)
attach(burg)

standard.fit <- glm( burglaries ~ median.income , family = "poisson" )
summary( standard.fit )

overdispersed.fit <- glm( burglaries ~ median.income , family = "quasipoisson")
summary( overdispersed.fit )

negative.fit <- glm.nb (burglaries ~ median.income )
summary(negative.fit)


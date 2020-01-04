
library(ggmcmc)
data(radon)

s.radon.short <- radon$s.radon.short
head(s.radon.short)

S <- ggs(s.radon.short)

head(S)
dim(S)

ggmcmc(S)

ggs_density(S)

ggs_traceplot(S, family = "alpha|beta")

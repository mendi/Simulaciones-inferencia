N <- 500000
mu_t <- 50
sigma_t <- 20
muestras <- 10000
n <- 30
alfa <- 0.05
z <- qnorm(alfa/2, lower.tail = F)

universo1 <- rnorm(N, mu_t, sigma_t)

mean(universo1)
sd(universo1)

lci_llenar <- 1:muestras
lcs_llenar <- 1:muestras

for(i in 1:muestras){
        muestra <- sample(universo1, n)
                lci_llenar[i] <- mean(muestra)-z*sd(muestra)/sqrt(n)
                lcs_llenar[i] <- mean(muestra)+z*sd(muestra)/sqrt(n)
}

resultados <- data.frame(lci=lci_llenar,lcs=lcs_llenar)
head(resultados)

library(dplyr)
ver <- resultados$lci<=mean(universo1) & resultados$lcs>=mean(universo1)
class(ver)

mean(ver)

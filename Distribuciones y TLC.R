# Distribución normal

muestra <- rnorm(100000, 200, 10)

a <- density(muestra)
plot(a)

estandar <- (muestra - mean(muestra))/sd(muestra)

sd(estandar)

# Probabilidad aproximada por simulación: P(X >= 215, o, X <= 185)

cuales <- muestra >= 215 | muestra <= 185
mean(cuales)


# Teorema del Límite Central
rm(base)
mu <- 0
sigma <- 1
n <- 50
universo <- rnorm(100000, mu, sigma)
variabless <- 1:n
base <- 1:n
muestras <- 1000

        
for (i in 1:muestras) {
        variabless <- sample(universo, n)
        base <- data.frame(base, variabless)
}
colnames(base) <- c("x_i", paste0(rep("muestra", muestras),1:muestras))

medias <- 1:muestras
varianzas <- 1:muestras

for (i in 1:muestras) {
        medias[i] <- mean(base[, i+1])
        varianzas[i] <- var(base[, i+1])
}


chi_2 <- (n-1)*varianzas/sigma
plot(density(medias))
plot(density(chi_2))



# Otra perspectiva

simulaciones <- 10000
medias <- 1:simulaciones
desvEst <- 1:simulaciones

for (i in 1:simulaciones) {
        medias[i] <- mean(rnorm(simulaciones, mu, sigma))
        desvEst[i] <- sd(rnorm(simulaciones, mu, sigma))
}

a <- density(medias)
plot(a)

# Distribución normal exacta
x <- seq(-3.4, 3.4, by = .01)
normal <- dnorm(x)

plot(x, normal, type = "line")

# Distribución exponencial

expo <- rexp(simulaciones, 1)

a <- density(expo)
plot(a)



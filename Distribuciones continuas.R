# Falcao anota 5 goles cada 4 partidos. ¿Cuál es la probabilidad de que no anote goles en los
# próximos 2 partidos?

x <- seq(0, 10, by = 0.1)
alpha <- 5/4

fx <- alpha*exp(-alpha*x)
plot(x, fx, type = "histogram")

Fx <- 1-exp(-alpha*x)
plot(x, Fx, type ="lines")
lines(x, fx)

# Cálculo de la probabilidad pedida P(x > 2):

1-exp(-alpha*2)

# Ahora traduciendo el tiempo de partidos a minutos:

alphamin <- 5/360

minutos <- seq(0, 2000, by = 10)

fmin <- alphamin*exp(-alphamin*minutos)
plot(minutos, fmin)

Fmin <- 1-exp(-alphamin*minutos)
plot(minutos, Fmin)
points(minutos, fmin)

# Calculando la misma probabilidad:

1-exp(-alphamin*180)

# Calculando percentiles. A un servicio de urgencias llegan en promedio 7 pacientes por accidente de
# tránsito al día. Calclule la hora en que ocurrirá el 75% de las primeras llegadas cada día.

alphaurg <- 7

percentil75 <- log(1-0.75)/-alphaurg
percentil75*12

# Distribución beta

x <- seq(0, 1, by = 0.01)
fx1 <- dbeta(x, 1, 5, 0)
fx2 <- dbeta(x, 5, 1, 0)
fx3 <- dbeta(x, 3, 3, 0)
fx4 <- dbeta(x, 4, 4, 0)
plot(x, fx1, type = "lines")
lines(x, fx2, col = 7)
lines(x, fx3, col = 3)
lines(x, fx4, col = 1)


1-pgamma(20, 2, 1/4)

# Ejemplo 6.18 de Walpole: P(X < 2)

pgamma(1, 2, 5)

# Ejemplo 6.19 de Walpole: P(X <= 60)

x <- 60
        alfa <- 5
        beta <- 10
pgamma(x, alfa, 1/beta)

# Ejemplo 6.20 de Walpole: P(X >= 20)

x <- 20
alfa <- 2
beta <- 4
1-pgamma(x, alfa, 1/beta)

# Ejemplo 6.21 de Walpole: 

x <- 6
alfa <- 1
beta <- 4
1-pgamma(x, alfa, 1/beta)

pgamma()

# Ejercicio 6.40

x <- 9
alfa <- 2
beta <- 3
1-pgamma(x, alfa, 1/beta)

# Ejercicio 6.41

alfa <- 2
beta <- 1

pgamma(2.4, alfa, 1/beta) - pgamma(1.8, alfa, 1/beta)

# Ejercicio 6.42

# a)
x <- 1
alfa <- 2
beta <- 1/2
pgamma(x, alfa, 1/beta)

# b)

x <-2
1 - pgamma(x, alfa, 1/beta)


alfa <- 10
beta <- 1/5
pgamma(1, alfa, 1/beta)

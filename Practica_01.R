##############################################
#######          Practica N-1          #######
##############################################

# Ejercicio 1

n <- 100
p <- 0.5
x <- seq(0,10)
res <- dbinom(x, size=n, prob=p)
plot(x, res, type = 'l', xlab="Valor", ylab="Probabilidad")

acum <- numeric(101)
for(i in 0:100){
  acum[i+1] <- sum(dbinom(x[0:i], size=n, prob=p))
}

plot(seq(0,100), acum)

# Histograma
hist(rbinom(100, size=n, prob=p))


# Ejercicio 2

n <- 1000000
p <- 

Repetir el ejericico , p=1/2 e^-5

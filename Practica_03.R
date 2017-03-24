#############################################################
#########          Siniestralidad Agregada          #########
#########         Diego Paul Huaraca Shagñay        #########
#############################################################

# Distribución compuesta por simulación

# Inicialización de parámetros
n <- 1000 # numero de asegurados
rp <- numeric(n) # almacena el numero de siniestros
coste <- numeric(n) # almacena el costo de los siniestros
lambda <- 0.4 # numero medio de siniestros

# Función que toma los parámetros de la log-normal y calcula la media y varianza de
# la normal relacionada, mismos que se emplean para simular el costo del siniestro.
par_log <- function(ux,s2x){
  s2y <- log((s2x+ux^2)/ux^2) # varianza
  uy <- (4*log(ux)-log(s2x+ux^2))/2 # media
  return(list(mu = uy, sigma2 = s2y))
}

media <- 1200 # media de la log-normal
varianza <- 90000 # varianza de la log-normal
mu_y <- par_log(media, varianza)[[1]]
sigma2_y <- par_log(media, varianza)[[2]]

# Siniestralidad agregada
for(i in 1:n){
  rp[i] <- rpois(1, lambda)
  cte <- 0
  if(rp[i]!=0){
    for(j in 1:rp[i]){
      cte <- cte + exp(qnorm(runif(1), mean = mu_y, sd = sqrt(sigma2_y)))
    }
  }else{
    cte <- 0
  }
  coste[i] <- cte
}

cbind(rp,coste)

hist(coste, main="Siniestralidad Agregada", ylab="Frecuencia", xlab="Costo")

plot(seq(1:n),coste, type = 'l', main="Costo Siniestros", xlab="Asegurado", ylab="Costo")



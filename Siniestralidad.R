#################################################################
##########          Siniestralidad Agregada            ##########
#################################################################

# El cálculo de la prima pura de siniestralidad depende de dos procesos
# - Numero de siniestros (frecuencia de reclamos): Nt
# - Coste medio de los siniestros: Yi

library(magrittr)

set.seed(12345)
n <- 1000 # tamaño de cartera
lambda <- 1.5 # siniestros promedio
u <- 1200 # media muestral
s <- 900 # varianza muestral
res <- rpois(n, lambda) # simulación siniestros sobre su cartera
res %>% table() %>% barplot()
p_siniestros <- dpois(seq(0,10), lambda = lambda)

# ux, s2x: valores obtenidos por datos historicos
# uy, s2y: parametros buscados para simulacion

par_log <- function(ux,s2x){
      s2y <- log((s2x+ux^2)/ux^2) # varianza
      uy <- (4*log(ux)-log(s2x+ux^2))/2 # media
      return(list(mu=uy,sigma2=s2y))
}

# Simulacion Log-Normal
mu_y <- par_log(1200,900)[[1]]
sigma2_y <- par_log(1200,900)[[2]]
cuantia <- exp(qnorm(runif(1000), mean = mu_y, sd = sqrt(sigma2_y)))

# Siniestralidad
S <- mean(res)*mean(cuantia) # prima pura

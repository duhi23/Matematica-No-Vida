


library(fitdistrplus)
library(dplyr)
library(readr)

setwd("/Users/diego/Google Drive/UAH/Asignaturas/Matemática no Vida o de los Seguros Generales/Scripts")
list.files()
data <- read.table("autos.csv", header=TRUE, sep=";")[,-c(1,3)]
summary(data)
colnames(data) <- c("Poliza", "Siniestros", "Genero", "Zona", "Exp_med", "Exp_max", "Edad", "Cobertura",
                    "Potencia")

# tabla siniestros
table(data[,"Siniestros"])

# media siniestros
mean(data[,"Siniestros"])

# funcion varianza corregida
var_c <- function(x){
      return(var(x)*(length(x)-1)/length(x))
}

# calculo varianza
var_c(data[,"Siniestros"])

a_pois <- fitdist(data[,"Siniestros"],"pois", method="mme")
summary(a_pois)

a_nbin <- fitdist(data[,"Siniestros"],"nbinom", method="mme")
summary(a_nbin)

gofstat(a_pois, fitnames = "poisson")
gofstat(a_pois, fitnames = "poisson")



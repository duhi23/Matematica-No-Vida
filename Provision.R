##################################################################
##########          Diego Paul Huaraca Shagñay          ##########
##########     Estimación de reservas - Chain Ladder    ##########
##########         MACAF - Universidad de Alcala        ##########
##################################################################


# Creamos una tabla con valores aleatorios
data <- data.frame(fila=rep(1:10, times=10), columna=rep(1:10, each=10), 
                   valor=sample(seq(10000,50000), size=100, replace = TRUE))
data$valor <- ifelse(data$fila + data$columna > 11, NA, data$valor)

# Convertimos la tabla en un triangulo
triangulo <- as.triangle(data, origin="fila", dev="columna", value="valor")

# Triangulo acumulado
Acum <- function(triangulo){
      mat <- t(apply(triangulo, 1, cumsum))
      dimnames(mat) <- dimnames(triangulo)
      class(mat) <- c("triangle", "matrix")
      return(mat)
}

Acum(triangulo)

Desacum <- function(triangulo){
      mat <- cbind(triangulo[,1], t(apply(triangulo, 1, diff)))
      dimnames(mat) <- dimnames(triangulo)
      class(mat) <- c("triangle", "matrix")
      return(mat)
}

Desacum(triangulo)

# Graficamos las rectas
plot(Acumulado, xlab="Periodo", ylab="Pagos")

# Codigo calculo de los fk
fvalor <- function(matriz){
      n <- ncol(matriz)
      f <- numeric(n-1)
      MA <- Acum(matriz)
      for(i in 1:(n-1)){
            f[i] <- sum(MA[c(1:(n-i)),i+1])/sum(MA[c(1:(n-i)),i])
      }
      return(f)
}

# Obtencion de fk para la matriz acumulada
fk <- fvalor(triangulo)

# Codigo para completar el triangulo anterior usando los fk obtenidos
completar <- function(matriz, vector){
      n <- nrow(matriz)
      Ri <- numeric(n)
      MA <- Acum(matriz)
      for(i in 2:n){
            for(j in (n-i+2):n){
                  MA[i,j] <- MA[i,j-1]*vector[j-1]
            }
            Ri[i] <- MA[i,n] - MA[i, n+1-i]
      }
      return(list(Matriz=MA, reserva=sum(Ri)))
}

# Obtencion de la matriz
completar(triangulo, fk)


# Errores estimacion por fk
errores <- function(matriz, vector){
      n <- nrow(matriz)
      MA <- Acum(matriz)
      ME <- matrix(0, ncol=n, nrow=n)
      MERROR <- matrix(0, ncol=n, nrow=n)
      VE <- numeric(n*(n+1)/2)
      ME[,1] <- MA[,1]
      for(j in 2:n){
            ME[,j] <- ME[,j-1]*vector[j-1]
      }
      k <-1
      for(i in 1:n){
            for(j in 1:n){
                  if(i+j<=n+1){
                        #VE[k] <- MA[i,j] - ME[i,j]
                        MERROR[i,j] <- MA[i,j] - ME[i,j]
                        #k <- k+1
                  }
            }
      }
      for(i in 1:n){
            for(j in 1:n){
                  if(i+j<=n+1){
                        VE[k] <- Desacum(MERROR)[i,j]
                        k <- k+1
                  }
            }
      }
      return(list(vec=VE, mat=MERROR))
}

errores(triangulo, fk)

# Bootstrap No Parametrico

BootMack <- function(matriz, iter){
      n <- nrow(matriz)
      Rk <- numeric(iter)
      Mk <- matriz
      for(k in 1:iter){
            fk <- fvalor(Mk)
            Rk[k] <- completar(Mk, fk)$reserva
            for(i in 1:n){
                  for(j in 1:n){
                        if(i+j<=n+1){
                              Mk[i,j] <- Mk[i,j] + sample(errores(Mk, fk)$vec, size=1)/10
                        }
                  }
            }
      }
      return(Rk)
}

# Vector resultados
res <- BootMack(triangulo, 100)

# Histograma
hist(res)

# Cuartiles
quantile(res, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))



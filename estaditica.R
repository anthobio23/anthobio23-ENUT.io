## Teorema del limite central media, y distribucion binomial
genera <- function(dis, tamMuestra) {
  if (dist == "binom") {
    return(rbinom(tamMuestra, 100, 0.4))
  } else {
    return(rexp(tamMuestra,100))
  }
}

## declaracion de variables
tamMuestra <- 1000
numMuestras <- 600
dist <- "binom"

## definimos una matriz la cual almacenara las muestras que se generen.
mx <- matrix(, nrow = numMuestras, ncol = tamMuestra)

## creamos un vector para el almacenamiento de las medias
mediax <- vector()

## bucle for para generar muestras aleatorias
for (i in 1:numMuestras) {
  x <- genera(dist, tamMuestra)
  mx[i,] <- x
  mediax[i] <- mean(x)
}


print(c(mediax[numMuestras], mediax[numMuestras - 1], mediax[numMuestras -2],
        mediax[numMuestras - 3], mediax[numMuestras - 4], mediax[numMuestras - 5]))

par(mfrow = c(3, 3))
hist(mx[numMuestras, ], probability = T)
hist(mx[numMuestras - 1, ], probability = T)
hist(mx[numMuestras - 2, ], probability = T)
hist(mx[numMuestras - 3, ], probability = T)
hist(mx[numMuestras - 4, ], probability = T)
hist(mx[numMuestras - 5, ], probability = T)
hist(mediax, probability = T)


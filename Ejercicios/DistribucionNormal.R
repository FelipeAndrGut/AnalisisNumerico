#install.packages("pracma")
#library(pracma)

x <- seq(-3.09, 3.09, by=0.01)

mean <- sum(x) / 619

variance <- sum((x - mean) ^ 2) / 619

y <- (1/sqrt(2*pi*variance)) * exp((-(x-mean)^2 / 2*variance))
                                  
AUC <- trapz(x,y)

print(paste("Área debajo de la curva es:"))
AUC

dat <- data.frame(cbind(x, y))
sc <-plot(dat, pch=20, cex=1, col = "purple", asp=1, xlab="X", ylab="Y", main="Distribución normal") 

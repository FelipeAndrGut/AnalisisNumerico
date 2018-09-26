#install.packages("Deriv")
#library(Deriv)

x <- c(1.8, 1.9, 2.0, 2.1, 2.2)
h <- c(0.1, 0.01, 0.001, 0.0001, 0.00001)

s <- expression(x*exp(x))

print(paste("Derivada exacta de xe^x: "))
derivada = eval(D(s, 'x'))

f <- function(num){
  return(num*exp(num))
}
  
for (i in h){
  funcion4.4 = (1/(2*i)) * (-3*f(x) + 4*f(x+i) - f(x-(2*i)))
  print(paste("funcion4.4 con valor de h:", i, " es:"))
  print(paste(funcion4.4))
  print(paste("errores de funcion4.4 con valor de h:", i, " a comparación de la derivada exacta son:"))
  print(paste(derivada - funcion4.4))
}

for (i in h){
  funcion4.5 = (1/(2*i)) * (f(x+i) - f(x-i))
  print(paste("funcion4.5 con valor de h:", i, " es:"))
  print(paste(funcion4.5))
  print(paste("errores de funcion4.5 con valor de h:", i, " a comparación de la derivada exacta son:"))
  print(paste(derivada - funcion4.5))
}






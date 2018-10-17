f <- function(x)
{
  return(2+(exp(-x^3)*(exp(1)-2)))
}

h <- 0.001
x <- seq(-0.1, 0.1, by = h)

pendientes <- c()

for(i in 0:length(x))
{
  pendientes[i] = abs((f(x[i]+h)-f(x[i])) / h)
}

plot(pendientes, pch='|')

pendientes

print(paste("La cota es de ", pendientes[length(pendientes)]))

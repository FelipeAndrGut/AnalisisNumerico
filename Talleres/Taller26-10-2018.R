list.of.packages <- c("phaseR", "pracma")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(phaseR)
library(pracma)

f<-function(fcn,x,y){
  return(eval(fcn))
}

# Solo para prueba con dy=x+y, y(0)=1
obtenerErrorAbsoluto<-function(x,y){
  solucion=exp(x)*((-x*exp(-x))-exp(-x)+2)
  return(abs(y-solucion))
}

graficarCampoPendiente<-function(x0, xn, y0, yn, fcn, numpendientes, metodo){
  apma1 <- function(t, y, parameters){
    a <- parameters[1] 
    dy <- a*(f(fcn, t, y))
    list(dy)
  } 
  apma1.flowField <- flowField(apma1, x = c(x0, xn), 
                               y   = c(y0, yn), parameters = c(1), 
                               points = numpendientes, system = "one.dim", 
                               add = FALSE, xlab = "x", ylab = "y", 
                               main = metodo)
  grid()
}

graficarSolucionNumerica<-function (x, y){
  points (x, y, pch=20, col="blue")
  for (i in 2:length(x)){
    segments(x[i-1], y[i-1], x[i], y[i], col="red")
  }
}

rrk4<-function(dy, ti, tf, y0, h, graficar=TRUE, numpendientes=10){
  t<-seq(ti, tf, h)
  y<-c(y0)
  cat("x    |y         |k1        |k2        |k3        |k4       |error absoluto\n")
  for(i in 2:length(t)){
    k1=h*f(dy, t[i-1], y[i-1])
    k2=h*f(dy, t[i-1]+h/2, y[i-1]+k1*(0.5))
    k3=h*f(dy, t[i-1]+h/2, y[i-1]+k2*(0.5))
    k4=h*f(dy, t[i-1]+h, y[i-1]+k3)
    y<-c(y, y[i-1]+1/6*(k1+2*k2+2*k3+k4))
    cat(t[i-1]," | ", y[i-1]," | ",k1," | ",k2," | ",k3," | ",k4," | ",obtenerErrorAbsoluto(t[i-1],y[i-1]),"\n")
  }
  if (graficar){
    graficarCampoPendiente(min(t), max(t), min(y), max(y), dy, numpendientes, "RK4")
    graficarSolucionNumerica(t, y)
  }
  rta<-list(w=y, t=t)
}

rrk3<-function(dy, ti, tf, y0, h, graficar=TRUE, numpendientes=10){
  t<-seq(ti, tf, h)
  y<-c(y0)
  cat("x    |y         |k1         |k2        |k3       |error absoluto\n")
  for(i in 2:length(t)){
    k1=h*f(dy, t[i-1], y[i-1])
    k2=h*f(dy, t[i-1]+h/2, y[i-1]+k1*(0.5))
    k3=h*f(dy, t[i-1]+h, y[i-1]-k1+2*k2)
    y<-c(y, y[i-1]+1/6*(k1+4*k2+k3))
    cat(t[i-1]," | ", y[i-1]," | ",k1," | ",k2," | ",k3," | ",obtenerErrorAbsoluto(t[i-1],y[i-1]),"\n")
  }
  if (graficar){
    graficarCampoPendiente(min(t), max(t), min(y), max(y), dy, numpendientes, "RK3")
    graficarSolucionNumerica(t, y)
  }
  rta<-list(w=y, t=t)
}

dy <- function(x, y) {x+y+1-x^2}

metodoEuler <- function(dy, h, x0, y0, xf)
{
  N = (xf - x0) / h
  x = y = numeric(N+1)
  x[1] = x0; 
  y[1] = y0;
  i = 1
  while (i <= N)
  {
    x[i+1] = x[i]+h
    y[i+1] = y[i]+(h*dy(x[i],y[i]))
    i = i+1
  }
  return (data.frame(X = x, Y = y))
}

graficaEuler <- function(en)
{
  xlim0 <- en$X[1]
  ylim0 <- en$Y[1]
  xlimN <- en$X[nrow(en)]
  ylimN <- en$Y[nrow(en)]
  xx <- c(xlim0, xlimN+0.2); yy <- c(round(ylim0, digits = 0), (round(ylimN, digits = 0)) + 1)
  vectorfield(dy, xx, yy, scale = 0.03)
  for (xs in seq(xlim0, xlimN, by = 0.25)) 
  {
    solField <- rk4(dy, xlim0, xlimN+1, xs, 100)
    lines(solField$x, solField$y, col="purple")
  }
  sol <- rk4(dy, xlim0, xlimN, ylim0, 100)
  lines(sol$x, sol$y, col="black", type= "p")
}

  #Punto3

e3 <- metodoEuler(dy, 0.1, 0, 1, 2)

e3

graficaEuler(e3)

e3Exact <- euler_heun(dy, 0, 2, 1, n = 20)

errores3 <- data.frame(X = e3$X, Y = 0)

for (i in 1:nrow(e3))
{
  errores3$Y[i] <- abs(e3Exact$y[i]-e3$Y[i])
}

errores3

  #Punto4: 

fPunto4 <- function(xP40, yP40, h, m)
{
  xP4 = yP4 = numeric(m+1)
  xP4[1] = xP40; 
  yP4[1] = yP40;
  
  for (i in 1:m)
  {
    K1 <- h * dy(xP4[i], yP4[i])
    K2 <- h * dy(xP4[i] + h, yP4[i] + K1)
    yP4[i+1] <- yP4[i] + (1/2) * (K1 + K2)
    xP4[i+1] <- xP4[i] + h
  } 
  return (data.frame(X = xP4, Y = yP4))
}

e4 <- fPunto4(0, 1, 0.1, 20)

e4

graficaEuler(e4)

  #Punto5: Las gráficas se parecen aunque los números son diferentes 

    #PorMétodoDeEulerVariado

metodoEulerv <- function(dy, h, x0, y0, xf)
{
  N = (xf - x0) / h
  x = y = numeric(N+1)
  x[1] = x0; 
  y[1] = y0;
  i = 1
  while (i <= N)
  {
    x[i+1] = x[i]+h
    y[i+1] = y[i]+((h/2)*(dy(x[i],y[i]) + dy(x[i+1], y[i+1])))
    i = i+1
  }
  return (data.frame(X = x, Y = y))
}

e5v = metodoEulerv(dy, 0.1, 0, 1, 1)

e5v

plot(e5v$X, e5v$Y, col="purple")
lines(e5v$X, e5v$Y, col="purple")

    #PorMétodoDeEuler

e5 <- metodoEuler(dy, 0.1, 0, 1, 1)

e5

plot(e5$X, e5$Y, col="purple")
lines(e5$X, e5$Y, col="purple")

    #Comparación de ambos
errores5 <- data.frame(X = e5$X, Y = 0)

for (i in 1:nrow(e5))
{
  errores5$Y[i] <- abs(e5v$Y[i]-e5$Y[i])
}

errores5

  #Punto7: Los números de los resultados por medio del método de runge-Kutta, para un valor final de x de 1, 
  #son mayores que los resultados por medio del método de Euler con un valor de  y las gráficas son bastante diferente. 

    #PorMétodoDeRunge-Kutta

r74<-rrk4(expression(x+y+1-x^2), 0, 1, 1, 0.1)

r73<-rrk3(expression(x+y+1-x^2), 0, 1, 1, 0.1)

    #PorMétodoDeEuler

e7 <- metodoEuler(dy, 0.1, 0, 1, 1)

e7

graficaEuler(e7)

errores7rk3 <- data.frame(X = e7$X, Y = 0)

for (i in 1:nrow(e7))
{
  errores7rk3$Y[i] <- abs(r73$w[i]-e7$Y[i])
}

errores7rk3

errores7rk4 <- data.frame(X = e7$X, Y = 0)

for (i in 1:nrow(e7))
{
  errores7rk4$Y[i] <- abs(r74$w[i]-e7$Y[i])
}

errores7rk4

f = function(x)
{
  return (sin(x))
}

longitudCurva = function(a,b,n)
{
  cont = 0
  datos = seq(a,b,by=((b-a)/n))
  i = 1
  for(i in 1:(length(datos)-1))
  {
    deltaY = f(datos[i+1])-f(datos[i])
    deltaX = datos[i+1]-datos[i]
    
    calculo = sqrt(1+(deltaY/deltaX)^2)*deltaX
    cont = cont + calculo
  }
  return(c(cont,datos))
}

graficar = function(datos, a, b, paso, extra)
{
  x = seq(a,b,by=((b-a)/n))
  y = f(x)
  x2 = seq(a,b,by=((b-a)/m))
  y2= f(x2)
  par(mfrow=c(1,2))
  plot(x,y,type="l",asp=1,main="Curva General")
  plot(x2,y2,type="l",asp=1,main="Curva Aproximada")
  lines(datos,f(datos),col="purple",lwd=5)
}

a = 0
b = 2
n = 1000
m=10
l = longitudCurva(a,b,n)
longuitudAImprimir = (l[1] * pi)

cat(l[1])
datos = l[2:length(l)]

graficar(datos,a,b,n,m)

print(paste("La longuitud de la curva es: "))
print(paste(longuitudAImprimir))

datos

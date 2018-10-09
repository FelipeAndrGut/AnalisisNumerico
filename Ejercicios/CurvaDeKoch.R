CurvaDeKoch <- function(x)
{
  iterate <- function(T,i)
  {
    A = T[ ,1]; B=T[ ,2]; C = T[,3];
    if (i == 1)
    {
      d = (A + B)/2; 
      h = (C-d); 
      d = d-(1/3)*h;
      e = (2/3)*B + (1/3)*A; 
      f = (1/3)*B + (2/3)*A;
    }
    
    if (i == 2)
    {
      d = B; 
      e = (2/3)*B + (1/3)*C; 
      f = (2/3)*B + (1/3)*A;
    }
    
    if (i == 3)
    {
      d = (B + C)/2;
      h = (A-d); 
      d = d-(1/3)*h;
      e = (2/3)*C + (1/3)*B;
      f = (1/3)*C + (2/3)*B;
    }
    
    if (i == 4)
    {
      d = C;
      e = (2/3)*C + (1/3)*A;
      f = (2/3)*C + (1/3)*B;
    }
    
    if (i == 5)
    {
      d = (A + C)/2; 
      h = (B-d); d = d-(1/3)*h;
      e = (2/3)*A + (1/3)*C; 
      f = (1/3)*A + (2/3)*C;
    }
    
    if (i == 6)
    {
      d = A; 
      e = (2/3)*A + (1/3)*C;
      f = (2/3)*A + (1/3)*B;
    }
    
    if (i == 0)
    {
      d = A; 
      e = B;
      f = C;
    }
    
    Tnew = cbind(d,e,f)
    return(Tnew); #Return a smaller triangle.
  }
  
  draw <- function(T, col=rgb(26/255,17/255,16/255),border=rgb(16/255,52/255,166/255))
  {
    polygon(T[1,],T[2,],col=col,border=border)
  }
  
  Iterate = function(T,v,col=rgb(26/255,17/255,16/255),border=rgb(16/255,52/255,166/255))
  {
    for (i in v) T = iterate(T,i);
    draw(T,col=col,border=border);
  }
  
  #Construcción del triángulo inicial:
  A = matrix(c(1,0),2,1);
  B = matrix(c(cos(2*pi/3), sin(2*pi/3)),2,1);
  C = matrix(c(cos(2*pi/3),-sin(2*pi/3)),2,1);
  T0 = cbind(A,B,C);
  
  plot(numeric(0),xlim=c(-1.1,1.1),ylim=c(-1.1,1.1),axes=FALSE,frame=FALSE,ann=FALSE);
  par(mar=c(0,0,0,0),bg=rgb(1,1,1));
  par(usr=c(-1.1,1.1,-1.1,1.1));
  
  #Draw snowflake:
  for (i in 0:x) for (j in 0:x) for (k in 0:x) for (l in 0:x) Iterate(T0,c(i,j,k,l));
}

leerEntero <- function()
{ 
  n <- readline(prompt="Digite un número entre 0 y 6: ")
  n <- as.integer(n)
  if (is.na(n)){
    n <- leerEntero()
  }
  return(n)
}

x <- leerEntero()

CurvaDeKoch(x);

longitud <- 3 * 0.866 * (4/3)^(x+1)

print(paste("La longuitud de la curva es: "))
print(paste(longitud))

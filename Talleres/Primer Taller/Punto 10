import math

"""
Este método es hecho para una funcion X cualquiera que se pueda meter en la funcion
"""

def funcionDelMetodoNewton(valor):
    return math.cos(valor) - math.pow(valor,3)

def funcionDerivada (valor):
    return -math.sin(valor) - (3*valor*valor)

def metodoNewton(dato, n, aux, divergencia):
    if divergencia == 100:
        print("La ecuacion no converge")
        return
    aux = dato
    dato = dato - (funcionDelMetodoNewton(dato)/funcionDerivada(dato))
    error = abs((dato-aux)/dato)
    if(dato!=aux):
        print(" Repeticion #"+str(n)+" VALOR= "+str(dato)+ " Error = "+str(error)+"\n")
        metodoNewton(dato, n+1, aux, divergencia+1)
    else:
        print(" Repeticion #"+str(n)+" VALOR FINAL= "+str(dato)+ " SIN ERROR")
        

def main():
    x = float(input("Ingrese el valor de X para resoler la ecuacion sin(x)+cos(x)+ x^3 = "))
    print("/n")
    metodoNewton(x,1,0,0)
    
main()

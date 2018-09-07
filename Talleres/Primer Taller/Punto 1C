from math import*
def CalcularDato(valor):
    return (-5*pow(valor,6)) + (3*pow(valor,4)) + (2*pow(valor,2)) - (4*valor)

def CalcularDatoDerivada(valor):
    return (-30*pow(valor,5)) + (12*pow(valor,3)) + (4*valor - 4) 

def calcularOperaciones(ecuacion):
    v = 0
    for i in range (0, len(ecuacion)):
        if (ecuacion[i] == '+' or ecuacion[i] == '-'):
            v = v + 1
    return v

def main():

    ecuacion = input("Ingrese la ecuación dada: ")
    dato = int(input("Ingrese el valor para la ecuación -5x^6+3x^4+2x2-4x = "))
    print("El resultado de la ecuación es = "+ str(CalcularDato(dato)))
    print("El resultado de la ecuación derivada es = "+ str(CalcularDatoDerivada(dato)))
    print("La cantidad de operaciones minimas necesarias para sacar el resultado es = "+str(calcularOperaciones(ecuacion)))
    

main()

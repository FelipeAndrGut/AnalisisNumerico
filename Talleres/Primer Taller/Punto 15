#include <iostream>
#include <string>
#include <math.h>

double f(double x) //Ver punto A en la zona de comentarios, bajo el codigo
{
	double res = 5*x - exp(x) -1;
	return res;
}

double* fixPoint(double num, int tam)
{
	double *res  = new double [tam];
	res[0] = num;	

	for (int i = 1; i < tam; ++i)
		res[i] = (exp(res[i-1])/5) - 1/5;

	return res;
}

int main()
{
	std::cout.precision(8);
	std::cout.setf(std::ios::fixed);
	
	double a = 0.0, b = 0.0, x = 0.0;
	int cantIter = 0;

	tag:std::cout<<"Por favor digite el primer valor del intervalo."<<std::endl;
	std::cin>>a;
	std::cout<<"Por favor digite el ultimo valor del intervalo."<<std::endl;
	std::cin>>b;

	if (f(a) > 0 || f(b) < 0)
	{
		std::cout<<"Por favor digite valores iniciales diferentes; no existe una raiz entre estos."<<std::endl;
		std::cout<<std::endl;
		
		goto tag;
	}
	else
	{
		std::cout<<"Por favor digite el valor con el cual empezar."<<std::endl;
		std::cin>>x;				
		std::cout<<"Por favor digite la cantidad de iteraciones que desea."<<std::endl;
		std::cin>>cantIter;

		double *puntos = new double [cantIter];

		puntos = fixPoint(x, cantIter);

		for (int i = 0; i < cantIter; ++i)
			std::cout<<"iteracion #"<<i<<": "<<puntos[i]<<std::endl;

		std::cout<<"El error de truncamiento en el ultimo valor es de: "<<trunc(puntos[cantIter])<<std::endl;
	}	
}

/*---------------------------------------------------------------------------------------------------
a: Se empieza por separar la integral en una resta de integrales, de 5 y -e^u respectivamente. A la segunda integral, se factorisa el -1 para que
sea la integral de e^u solamente. Asi, integramos en ambas para obtener 5u y e^u*(-1), dandonos el resultado a evaluar entre 0 y x: 5u-e^u
Al evaluar, el primer termino es 5x-e^x, y el segundo es 0-e^0, el cual al simplificarse y tomar en cuenta que debe restarcele al primero, nos
da la ecuacion resultante 5x-e^x+1. Al igualar a 2, como dice el enunciado, despejamos de tal manera que nuestra ecuacion final es 5x-e^x-1 = 0.

b: Graficando nuestra funcion f(x) = 5x-e^x-1 en Desmos, obtenemos que las dos raices reales son 0.545 y 2.396

c: Es necesario expresar la funcion de tal manera que x sea igual a algo mas. Bajo esta idea, tenemos que x = (e^x)/5 - 1/5


*/






















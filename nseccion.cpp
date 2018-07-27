#include <iostream>
#include <string>
#include <cmath>

double f(double c)
{
	double res = (((9.8 * 68.1) / c) * (1 - std::exp(-(c/68.1)*10))) - 40;
	return res;
}

void validar(double c)
{
	std::cout<<std::endl;
	std::cout<<"Validación del último valor: "<<c*c<<std::endl;	
}

bool nseccion(double a, double b, double eps, int n, int iter)
{		
	bool noLlego = false;
	double c = 0.0;
		
	while (fabs(a - b) >= eps)
	{
		c = (a + b) / n;
		"En la iteración "<<iter<<": "<<std::endl;
		std::cout<<"\t"<<"El valor aproximado de la raíz es: "<<c<<std::endl;
		std::cout<<"\t"<<"El error de truncamiento es: "<<fabs(a - b)<<std::endl;

		if (f(c) == 0)
		{
			std::cout<<"El valor es: "<<c<<" en la última iteración: "<<iter<<std::endl;
			break;
		}
	
		if (f(a) * f(c) > 0)
			a = c;
		else if (f(a) * f(c) < 0)
			b = c;
	
		iter++;

		if (fabs(a - b) < eps && f(c) != 0)
			noLlego = true;
	}
	
	validar(c);

	return noLlego;
}

int main()
{
	std::cout.precision(8);
	std::cout.setf(std::ios::fixed);
	
	double a = 0.0, b = 0.0, eps = 0.0;
	int iter = 0, n = 0;

	tag:std::cout<<"Por favor digite el primer valor inicial."<<std::endl;
	std::cin>>a;
	std::cout<<"Por favor digite el segundo valor inicial."<<std::endl;
	std::cin>>b;

	if (f(a)*f(b) > 0)
	{
		std::cout<<"Por favor digite valores iniciales diferentes; no existe una raiz entre estos."<<std::endl;
		std::cout<<std::endl;
		
		goto tag;
	}
	else
	{
		std::cout<<"Por favor digite la cantidad de cifras significativas para el cálculo."<<std::endl;
		std::cin>>eps;
		std::cout<<"Por favor digite la cantidad de particiones (dos para emular el método de bisección)."<<std::endl;
		std::cin>>n;

		if (nseccion(a, b, eps, n, iter))
			std::cout<<"El método no pudo aproximarse más al valor genuino."<<std::endl;
	}

	return 0;
}

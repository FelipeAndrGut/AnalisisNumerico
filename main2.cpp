#include <iostream>
#include <string>
#include <math.h>

short int* formarPoly(int medida)
{
    short int *res = new short int [2];
    res[0] = medida;
    res[1] = -2;
    
    return res;
}

short int* multiplicarPoly(short int *polyLargo, short int *polyAncho, int tamL, int tamA)
{
    short int *res = new short int [tamL + tamA - 1];
    
    for (int i = 0; i < tamL + tamA - 1; ++i)
        res[0] = 0;
        
    for (int i = 0; i < tamL; ++i)
        for (int j = 0; j < tamA; ++j)
            res[i + j] += polyLargo[i] * polyAncho[j];
    
    return res;        
}

void coutPoly(short int *poly, int tam)
{
    for (int i = 0; i < tam; ++i)
    {
        std::cout << poly[i];
        if (i != 0)
        std::cout << "x^" << i;
        if (i != tam-1)
        std::cout << " + ";
    }
}

float roots(short int *polyAltura)
{
	int c = polyAltura[1];
	int b = polyAltura[2] * 2;
	int a = polyAltura[3] * 3;

	float discriminant = b*b - 4*a*c;
	float res = 0.1, x1 = 0.1, x2 = 0.1;
	
	if (discriminant < 0)
		std::cout<<"La solucion no se encuentra entre los numeros reales."<<std::endl;
	else if (discriminant == 0)
	{
		x1 = (-b + sqrt(discriminant)) / (2*a);
		return x1;
	}
	else if (discriminant > 0)
	{
		x1 = (-b + sqrt(discriminant)) / (2*a);
		x2 = (-b - sqrt(discriminant)) / (2*a);
		
		int aa = a * 2;
	
		if ((aa*x1 - (b*-1)) < 0)
			return x1;
		else if ((aa*x2 - (b*-1)) < 0)
			return x2;
	}

	if ((a*x1*x1 + b*x1 + c == 0) && (a*x2*x2 + b*x2 + c != 0) || (a*x1*x1 + b*x1 + c != 0) && (a*x2*x2 + b*x2 + c == 0))
 		std::cout<<"La respuesta ha sido validada."<<std::endl;
	else if ((a*x1*x1 + b*x1 + c != 0) && (a*x2*x2 + b*x2 + c != 0))
		std::cout<<"La respuesta no pudo validarse. Esta aproximada."<<std::endl;
}

int main ()
{
    /*int largo = 0, ancho = 0, altura = 0;
    
    std::cout<<"Por favor digite el largo de la lC!mina."<<std::endl;
    std::cin>>largo;
    std::cout<<"Por favor digite el ancho de la lC!mina."<<std::endl;
    std::cin>>ancho;*/
    
    int largo = 32, ancho = 24, altura = 0;
    short int *polyLargo = new short int [2], *polyAncho = new short int [2];
    
    polyLargo = formarPoly(largo);
    polyAncho = formarPoly(ancho);
    
    short int *polyX = new short int [2];
    polyX[0] = 0;
    polyX[1] = 1;
    
    int polyLargoTam = sizeof(polyLargo) / sizeof(polyLargo[0]);
    int polyAnchoTam = sizeof(polyAncho) / sizeof(polyAncho[0]);
    
    short int *polyDummy = new short int [polyLargoTam + polyAnchoTam - 1];
    
    polyDummy = multiplicarPoly(polyLargo, polyAncho, polyLargoTam, polyAnchoTam);
    
    int polyDummyTam = sizeof(polyDummy) / sizeof(polyDummy[0]);
    int polyXTam = sizeof(polyX) / sizeof(polyX[0]);
    
    short int *polyAltura = new short int [polyDummyTam + polyXTam - 1];
    
    polyAltura = multiplicarPoly(polyDummy, polyX, polyDummyTam, polyXTam);

    float res = roots(polyAltura);

    std::cout<<"La medida requerida es de: "<<res<<std::endl;
}

/* Mismo problema, resuelto por el metodo de completar cuadrados.
#include <iostream>
#include <string>
#include <math.h>

short int* formarPoly(int medida)
{
    short int *res = new short int [2];
    res[0] = medida;
    res[1] = -2;
    
    return res;
}

short int* multiplicarPoly(short int *polyLargo, short int *polyAncho, int tamL, int tamA)
{
    short int *res = new short int [tamL + tamA - 1];
    
    for (int i = 0; i < tamL + tamA - 1; ++i)
        res[0] = 0;
        
    for (int i = 0; i < tamL; ++i)
        for (int j = 0; j < tamA; ++j)
            res[i + j] += polyLargo[i] * polyAncho[j];
    
    return res;        
}

void coutPoly(short int *poly, int tam)
{
    for (int i = 0; i < tam; ++i)
    {
        std::cout << poly[i];
        if (i != 0)
        std::cout << "x^" << i;
        if (i != tam-1)
        std::cout << " + ";
    }
    std::cout<<"\n";	
}

float fullSquare(short int *poly)
{
	float c = poly[1];
	float b = poly[2] * 2;
	float a = poly[3] * 3;
	
	a /= a;
	b /= a;
	c /= a;

	c *=-1;

	float square = b / (2 * a);
	c += pow(square,2);

	float x1 = sqrt(c) + square;
	float x2 = (-1 * sqrt(c)) + square;

	if (24 * x1 - 224 < 0)
		return x1;
	else if (24 * x2 - 224 < 0)
		return x2;	
}

int main ()
{*/
    /*int largo = 0, ancho = 0, altura = 0;
    
    std::cout<<"Por favor digite el largo de la lC!mina."<<std::endl;
    std::cin>>largo;
    std::cout<<"Por favor digite el ancho de la lC!mina."<<std::endl;
    std::cin>>ancho;*/
    
    /*int largo = 32, ancho = 24, altura = 0;
    short int *polyLargo = new short int [2], *polyAncho = new short int [2];
    
    polyLargo = formarPoly(largo);
    polyAncho = formarPoly(ancho);
    
    short int *polyX = new short int [2];
    polyX[0] = 0;
    polyX[1] = 1;
    
    int polyLargoTam = sizeof(polyLargo) / sizeof(polyLargo[0]);
    int polyAnchoTam = sizeof(polyAncho) / sizeof(polyAncho[0]);
    
    short int *polyDummy = new short int [polyLargoTam + polyAnchoTam - 1];
    
    polyDummy = multiplicarPoly(polyLargo, polyAncho, polyLargoTam, polyAnchoTam);
    
    int polyDummyTam = sizeof(polyDummy) / sizeof(polyDummy[0]);
    int polyXTam = sizeof(polyX) / sizeof(polyX[0]);
    
    short int *polyAltura = new short int [polyDummyTam + polyXTam - 1];
    
    polyAltura = multiplicarPoly(polyDummy, polyX, polyDummyTam, polyXTam);

    float res = fullSquare(polyAltura);

    std::cout<<"La medida requerida es de: "<<res<<std::endl;
}
*/

/*----------------------------------------------------------------------------------------------------------------------------------------
1: Con el proceso llevado a cabo, la parte más crítica del proceso es la multiplicación de polinómios, ya que estos cálculos pueden variar 
con más frecuencia, gracias a la posibilidad de entradas hechas por el usuario, a la véz que las multiplicaciones requeridas para sus derivadas
se asumen dentro del código.

2: Los conocimientos requeridos para formular un modelo matemático son formular una ecuación que sea representativa de la situación en 
el mundo real, tener claro el método de resolución y cómo llevarlo a cabo en código.

3: La desventaga más significativa a la hora de obtener resultados por prueba y error es la pérdida de tiempo. Las medidas deseadas contienen más
de tres cifras significativas, y son distancias bastante pequeñas; la probabilidad que las personas probando cada combinación que se les ocurra
seán capaces de encontrar la respuesta óptima es abismal, lo que conlleva a no sólo mucho tiempo gastado en intentos, sino a tiempo perdido.

4: El error de truncamiento es más preocupante en general, por descartar las demás cifras reales sin concideración alguna, lo que puede llevar
a mayores errores que si se redondeara.

5: La ventaga de desarrollar un método númerico de forma computacional es que, como éstos se basan en procedimientos netamente lógicos y
aritméticos y las bastas capacidades de cómputo de los computadores actuales, hacen que una máquina pueda resolver estos problemas en meras
fracciones de fracciones del tiempo que le llevaría a un ser humano.

6: Es importante validar las respuestas para asegurarse que un proceso que sea capaz de terminar sin presentar errores sí esté retornando la
respuesta correcta.

*/

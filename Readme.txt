--------------------------------------------------------------------------------------------------------
Readme: fold.x
--------------------------------------------------------------------------------------------------------
Utilización del programa:
* Para compilar programa y subrutinas:
	gfortran -fcheck=all subrutinas.f90 foldbin.f90 -o foldbin.x

* Para ejecutar el programa:
	./foldbin.x

* Para ejecutar sismica.x con determinado input:
	./foldbin.x < entrada.inp
--------------------------------------------------------------------------------------------------------
Datos de entrada:
Hay una serie de consideraciones que se deben tener en cuenta:
*Los datos de entrada (Modo 2) deben estar dispuestos en dos
archivos llamados: 'R.in' y 'S.in' para receptores y fuentes
respectivamente.
*No debemos tener encabezado en ninguno de los archivos de
entrada.
*En ambos archivos debemos tener la posicion (en metros) en
las coordenadas x e y, en ese orden.
*En el Modo 2 se nos pedira, ademas, que especifiquemos la
separación entre receptores y fuentes (m) utilizadas durante la
adquisición. Por lo tanto es importante tener este dato presente
(en los datos Demo R.in y S.in fueron registrados con separaciones
de 50 metros).
*En la carpeta Demo hay un archivo 'entrada.inp' el cual se
puede modificar para ejecutar el programa mediante el modo 1
bajo los parametros deseados mas rapidamente.
--------------------------------------------------------------------------------------------------------
Libreria DISLIN:
*Para ejecutar el programa en su totalidad es necesario tener instalada
la libreria DISLIN. Toda la informacion necesaria se puede encontrar
en su sitio web:
https://www.dislin.de/index.html
--------------------------------------------------------------------------------------------------------
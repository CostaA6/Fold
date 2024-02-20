!==========================================================
Program Grafico
!==========================================================
USE DISLIN
!-------------------------
Implicit none
!-------------------------
Integer n, i, io, j, filas, columnas
Real Xmin, Xmax, Ymin, Ymax, Fmin, Fmax
Real, Allocatable :: x(:), y(:), fold(:)
Character*20 :: TXT
!-------------------------
!Archivos involucrados.
Open (20, file = 'PlotInput.dat', status = 'old')
Open (30, file = 'Reporte_Grafico.out') 
Open (40, file = 'Reporte.out') 
!==========================================================
!Importacion de archivos de entrada (output de: sismica.x)

Read(40,*) TXT,columnas,TXT,filas      !Tamaño de los datos
n = filas * columnas            !Numero de CMP

Allocate (x(n),y(n),fold(n))    !Memoria requerida.

DO i=1,n
	Read(20,*) X(i), Y(i), fold(i)
END DO
Rewind(20)
!==========================================================
!Valores limite de los arreglos involucrados.
Xmin = minval(X)
Xmax = maxval(X)
Ymin = minval(Y)
Ymax = maxval(Y)
Fmin = minval(fold)
Fmax = maxval(fold)
!-------------------------
write(30,*) 'X minimo:', Xmin
write(30,*) 'X maximo:', Xmax
write(30,*) 'Y minimo:', Ymin
write(30,*) 'Y maximo:', Ymax
write(30,*) 'Fold minimo:', Fmin
write(30,*) 'Fold maximo:', Fmax
!==========================================================
!Realizacion del grafico: (Subrutinas DISLIN)
call scrmod('revers')       !Color del fondo.
!Call metafl('CONS')        !Tipo de archivo de salida.
Call metafl('PNG')          !Tipo de archivo de salida.
call setfil('MapaFold')     !Nombre del archivo de salida.

call disini                 !Inicio de grafico.
call pagera()               !Forma de los bordes.
call hwfont()               !Fuente utilizada.
call titlin('FOLD',2)       !Titulo.
!call titlin(' ',4)         !Subtitulo.

call name('X-axis (m)','X') !Nombre del eje X.
call name('Y-axis (m)','Y') !Nombre del eje Y.
call name('FOLD','FOLD')    !Escala de colores.
call intax()
call axspos(400,1850)       !Posicion de los ejes.
call ax3len(2100,1400,1400) !Longitud de los ejes.

call graf3(Xmin,Xmax,Xmin*2,50.0,Ymin,Ymax,Ymin*2,50.0,Fmin,Fmax,Fmin,1.0)                     !Limites de los ejes y paso.
call setres(70,70)          !Tamaño de los cuadrados.
call curve3(X,Y,Fold,n)     !Parametros del grafico.

call height(50)
call title()
call disfin                 !Final de grafico.
!==========================================================
END Program Grafico
!==========================================================

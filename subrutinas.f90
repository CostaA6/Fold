!===============================================================
MODULE subrutinas
Implicit none
Contains
!===============================================================
!SUBRUTINA: Geometria:
!Realiza un arreglo de coordenadas regulares, con la posicion
!de todos los puntos pertenecientes a una secuencia de hileras
!con una separacion entre cada punto de una misma hilera y una
!separacion entre las hileras de la geometria.

! INT  = Separacion entre puntos de una misma hilera.
! L_INT= Separacion entre las hileras.
! X0   = Posicion X del primer punto de la geometria.
! Y0   = Posicion Y del primer punto de la geometria.
! NL   = Numero de lineas dentro de la Geometria.
! N    = Numero de puntos dentro de una misma linea.
! XX   = Vector de salida para las coordenadas X
! YY   = Vector de salida para las coordenadas Y

Subroutine geometria(INT,L_INT,X0,Y0,NL,N,XX,YY)

Real*8 INT,L_INT,X0,Y0
Integer NL,N,i,j
Real*8,dimension(NL*N) :: XX,YY

DO j=1,NL
DO i=1,N
    XX((j-1)*N+i) = (i-1) * INT   + X0
    YY((j-1)*N+i) = (j-1) * L_INT + Y0    
END DO
END DO
Return
End subroutine geometria
!===============================================================
!SUBRUTINA: Longdata:
!Calcula la longitud de un determinado archivo utilizando la
!funcion IOSTAT de FORTRAN.

! io  = Contador para DO LOOP (Variable auxiliar).
! n   = Longitud del archivo.
! dat = Nomenclatura del archivo de entrada.

Subroutine longdat(io,n,dat)

Integer io,n,dat
io = 0
n  = 0
DO While (io == 0)
    Read(dat,*,IOSTAT=io)
    IF (io==0) n=n+1
END DO
Rewind(dat)

Return
End Subroutine longdat
!===============================================================
!SUBRUTINA: Readdata
! Realiza la lectura de un archivo compuesto por dos columnas
! con datos y los almacena en una nueva variable de salida.

! n     = Longitud del archivo a leer.
! ItemX = Variable de salida, datos de la primer columna.
! ItemY = Variable de salida, datos de la segunda columna.
! dat   = Nomenclatura del archivo de entrada.

Subroutine readdata(n,ItemX,ItemY,dat)
Integer n,dat,i
Real*8,dimension(n) :: ItemX, ItemY

DO i = 1,n
    Read(dat,*) ItemX(i), ItemY(i)
END DO
Rewind(dat)

Return
End Subroutine readdata
!===============================================================
!SUBRUTINA: MIDPOINTS
!Realiza el calculo de la ubicacion de todos los puntos medios
!dados entre cada punto de shot con cada punto de recepcion.

! nr = Longitud de los datos de Receptores.
! ns = Longitud de los datos de Shots.
! SX = Arreglo con las posiciones X de los shots.
! SY = Arreglo con las posiciones Y de los shots.
! RX = Arreglo con las posiciones X de los receptores.
! RY = Arreglo con las posiciones Y de los receptores.
! MX = Arreglo con las posiciones X de los CMP (Salida).
! MY = Arreglo con las posiciones Y de los CMP (Salida).

Subroutine midpoints(nr,ns,SX,SY,RX,RY,MX,MY)

Integer nr,ns,i,j
Real*8,dimension(ns) :: SX,SY
Real*8,dimension(nr) :: RX,RY
Real*8,dimension(ns*nr) :: MX,MY

DO j=1,ns
DO i=1,nr
    MX((j-1)*nr+i) = 0.50d0 * (SX(j)+RX(i))
    MY((j-1)*nr+i) = 0.50d0 * (SY(j)+RY(i))
END DO
END DO
Return
End subroutine midpoints
!===============================================================
!SUBRUTINA: Cmpcell
!Ubica las posiciones de de los midpoints dentro de una grilla
!(bingrid) de celdas con un lado de 25mts. Esta subrutina tambien
!es capaz de trabajar con posiciones reales.

! nr = Longitud de los datos de Receptores.
! ns = Longitud de los datos de Shots.
! B  = Lado de la celda BIN.
! MX = Arreglo con las posiciones X de los CMP.
! MY = Arreglo con las posiciones Y de los CMP.
! MI = Posicion en indice (i) dentro de la BIN GRID.
! MJ = Posicion en indice (j) dentro de la BIN GRID.

Subroutine cmpcell(nr,ns,B,MX,MY,MI,MJ)

Integer i,j,ns,nr
Real*8 B
Real*8,dimension(ns*nr) :: MX,MY
Integer,dimension(ns*nr) :: MJ,MI

DO i = 1,ns*nr
    MI(i) = ANINT((MX(i)/B)+0.50d0)
    MJ(i) = ANINT((MY(i)/B)+0.50d0)
END DO
Return
End subroutine cmpcell
!===============================================================
!SUBRUTINAS: zerogrid
!Fabrica una grilla inicial donde cada elemento de la misma
!es un cero, esto para posteriormente incrementar el valor de
!cada celda en funcion del fold que presenta.

! filas    = Numero de filas de la BINGRID.
! columnas = Numero de columnas de la BINGRID.
! BINGRID  = Arreglo con una grilla de valores cero (Salida).

Subroutine zerogrid(filas,columnas,BINGRID)

Integer filas,columnas,i,j
Integer,dimension(filas,columnas) :: BINGRID

DO i=1,filas
DO j=1,columnas
    BINGRID(i,j) = 0
END DO
END DO

End Subroutine zerogrid
!===============================================================
END MODULE subrutinas
!===============================================================

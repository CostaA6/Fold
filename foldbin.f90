!=========================================================
Program foldbin
!=========================================================
!Definicion de variables.
!-------------------------
USE subrutinas
!-------------------------
Implicit None
!-------------------------
Real*8 :: RI,RLI,SI,SLI,XR0,YR0,XS0,YS0,B
Integer :: io,n,i,j,nr,ns,dat,ENTRADA,Rdat,Sdat
Integer :: NCHANNEL,NLR,NSHOT,NLS,filas,columnas
Real*8,allocatable :: RXX(:),RYY(:),SXX(:),SYY(:)
Real*8,allocatable :: SX(:),SY(:),RX(:),RY(:),MXX(:),MYY(:)
Real*8,allocatable :: MX(:),MY(:),MXG(:),MYG(:)
Integer,allocatable:: MI(:),MJ(:),BINGRID(:,:)
!------------------------------------
!Archivos involucrados.
Open(10,file="R.in")
Open(20,file="S.in")
Open(50,file="Reporte.out")
Open(70,file="FOLD.out")
Open(80,file="PlotInput.dat")

Call System ("clear")
Print*,"<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<"
Print*,"Calculo del FOLD"
Print*,"<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<"
Print*,"Descipción:"
Print*,"Este programa realiza el calculo del fold para una"
Print*,"geometria ortoganal con una misma separacion entre"
Print*,"receptores y puntos de disparo."
Print*,"Ademas permite la obtencion del mapa de fold para"
Print*,"geometria utilizada."
Print*,"<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<"
Print*,""
Print*,"Seleccionar el modo con el que desea trabajar:"
Print*,""
Print*,"> Modo 1: Establesca los parametros de adquisicion"
Print*,"para desarrollar una geometria sintetica y obtendra"
Print*,"el calculo del fold para dicha geometria y un mapa"
Print*,"del mismo."
Print*,""
Print*,"> Modo 2: Utilice dos archivos de entrada nombrados"
Print*,"R.in y S.in con las ubicacionres de los receptores"
Print*,"y los shots respectivamente."
Print*,"Los archivos deben tener columnas con las posiciones"
Print*,"X e Y (en ese orden), sin formato y sin cabecera en"
Print*,"ambos casos."
Print*,""
Print*,"<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<"
Print*,"(1) Establecer parametro de adquisición."
Print*,"(2) Cargar datos de posiciones."
Print*,"<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<"
Read*, ENTRADA
Call System ("clear")
Print*,"<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<"
Print*,"Calculo del FOLD"
Print*,"<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<"
Print*,""
!========================================================
IF (ENTRADA == 1) THEN
!--------------------------
Open(30,file="Sintetico_R.in")
Open(40,file="Sintetico_S.in")
!========================================================
!Parametros de Adquisicion:
!------------------------------------
!RECEPTORES:
Print*,"Establecer los parametros de adquision:"
Print*,""
Print*,"> Numero de Canales por linea:"
Read*, NCHANNEL
Print*,"> Numero de Lineas:"
Read*, NLR
Print*,"> Numero de Shots por linea:"
Read*,NSHOT
Print*,"> Numero de Lineas:"
Read*,NLS
Print*,"> Separacion entre canales y puntos de disparo (Metros):"
Read*, RI
Print*,"> Separacion entre lineas receptoras (Metros)"
Read*, RLI
Print*,"> Separacion entre las lineas de Shot (Metros) "
Read*,SLI
Print*,"> Posicion del Receptor inicial (Metros)"
Print*,"    - X0"
Read*,XR0
Print*,"    - Y0"
Read*,YR0
Print*,"> Posicion del punto de shot inicial (Metros)"
Print*,"    - X0"
Read*,XS0
Print*,"    - Y0"
Read*,YS0
!------------------------------------
SI = RI
B  = RI*0.50d0

Call System ("clear")
Print*,"<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<"
Print*,"Calculo del FOLD"
Print*,"<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<"
Print*,"Modo 1: [Datos sinteticos]"

!========================================================
!Desarrollo del TEMPLATE:
!------------------------------------
!Receptores:
Allocate(RXX(NLR*NCHANNEL) , RYY(NLR*NCHANNEL))

Call geometria(RI,RLI,XR0,YR0,NLR,NCHANNEL,RYY,RXX)

DO i = 1,NLR*NCHANNEL
    Write(30,*) RXX(i),RYY(i)
END DO
Rewind(30)
!------------------------------------
!Fuentes:
Allocate(SXX(NLS*NSHOT) , SYY(NLS*NSHOT))

Call geometria(SI,SLI,YS0,XS0,NLS,NSHOT,SXX,SYY)

DO i = 1,NLS*NSHOT
    Write(40,*) SXX(i),SYY(i)
END DO
Rewind(40)
!========================================================
Rdat = 30
Sdat = 40
Print*, "Se crearon dos archivos con la geometria sintetica."
!========================================================
ELSE
Rdat = 10
Sdat = 20

Call System ("clear")
Print*,"<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<"
Print*,"Calculo del FOLD"
Print*,"<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<"
Print*,"Modo 2: [Datos reales]"
Print*,""
Print*,"Especifique la separación entre los receptores y las"
Print*,"fuentes, utilizada durante la adquisición (Metros):"
Read*, RI
B = RI*0.50d0

Call System ("clear")
Print*,"<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<"
Print*,"Calculo del FOLD"
Print*,"<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<"
Print*,"Modo 2: [Datos reales]"

END IF
!========================================================
!Longitud del INPUT.
!------------------------------------
Print*,""
Print*,"Caracteristicas del Input:"
Print*,""
Call longdat(io,nr,Rdat)
Print*,"> Numero de receptores:", nr
!------------------------------------
Call longdat(io,ns,Sdat)
Print*,"> Numero de disparos  :",ns
!=========================================================
!Lectura del INPUT. (SX - SY - RX - RY)
!------------------------------------
!Memoria de los ARRAY de entrada.
Allocate(SX(ns) , SY(ns)) !Memoria de S
Allocate(RX(nr) , RY(nr)) !Memoria de R
!------------------------------------
!Lectura de RX, RY.
Call readdata(nr,RX,RY,Rdat)
!------------------------------------
!Lectura de SX, SY.
Call readdata(ns,SX,SY,Sdat)
!========================================================
!Calculo de la ubicacion de MIDPOINTS.

Allocate(MX(ns*nr) , MY(ns*nr))

call midpoints(nr,ns,SX,SY,RX,RY,MX,MY)

Print*, "> Numero de midpoints :",ns*nr
!=========================================================
!Ahora voy a dividir cada elemento de MX y MY por B:
!------------------------------------
!B = RI/2

Allocate(MJ(nr*ns) , MI(nr*ns))

call cmpcell(nr,ns,B,MX,MY,MI,MJ)
!=========================================================
!BINGRID y Calculo del FOLD

!Numero de filas y columnas de la BINGRID.
filas = ANINT(MAXVAL((/MAXVAL(RX),MAXVAL(SX)/))/B)
columnas = ANINT(MAXVAL((/MAXVAL(RY),MAXVAL(SY)/))/B)

Print*,""
Print*,"Caracteristica del BIN GRID:"
print*, "> Filas       :", filas
Print*, "> Columnas    :", columnas
Print*, "> Lado del BIN:", B
Print*, ""

Allocate(BINGRID(filas,columnas))
call zerogrid(filas,columnas,BINGRID)
!------------------------------------
DO i=1,ns*nr
    BINGRID(MI(i),MJ(i)) = BINGRID(MI(i),MJ(i)) + 1 
END DO

!=========================================================
!Output
!-------------------------
!BINGRID (Numerico)
DO i=1,filas
    Write(70,*) BINGRID(i,:)
END DO

!BINGRID (Input para Grafico.f90)
DO i=1,filas
DO j=1,columnas
    !Write(80,*) (j*50) , (i*50), BINGRID(i,j)
    Write(80,*) B/2+B*(j-1) , B/2+B*(i-1), BINGRID(i,j)
END DO
END DO
!Numero de CMP (Input para Grafico.f90)
Write(50,*) "Columnas:",columnas,"      Filas:",filas
Write(50,*) ""
Write(50,*) "Caracteristicas del Input:"
Write(50,*) "> Numero de receptores:", nr
Write(50,*) "> Numero de disparos  :", ns
Write(50,*) "> Numero de midpoints :", nr*ns
Write(50,*) ""
Write(50,*) "Caracteristica del BIN GRID:"
Write(50,*) "> Filas       :", filas
Write(50,*) "> Columnas    :", columnas
Write(50,*) "> Lado del BIN:", B
!=========================================================
!GRAFICO
Print*, "¿Desea visualizar el mapa de FOLD obtenido?"
Print*, "(1) SI"
Print*, "(2) NO"
Read*, ENTRADA
!----------------------------
IF (ENTRADA == 1) THEN
Call System ("f90link -a Grafico")
ELSE
END IF
!=========================================================
End Program
!=========================================================

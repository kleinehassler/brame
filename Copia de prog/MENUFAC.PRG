*       *********************************************************
*       *                                                         
*       * 17/04/2014            MENUFAC.PRG             17:18:29  
*       *                                                         
*       *********************************************************
*       *                                                         
*       * Nombre del autor                                        
*       *                                                         
*       * Copyright (C) 2014 Nombre de compañía                   
*       * Dirección                                               
*       * Ciudad,     Código pos                                  
*       * País                                              
*       *                                                         
*       * Descripción:                                            
*       * Este PROGRAMA lo ha generado automáticamente GENMENU.    
*       *                                                         
*       *********************************************************


*       *********************************************************
*       *                                                         
*       *                    Definición de menú                   
*       *                                                         
*       *********************************************************
*

SET SYSMENU TO
SET SYSMENU AUTOMATIC

DEFINE PAD _41p113i4i OF _MSYSMENU PROMPT "Ventas" COLOR SCHEME 3 ;
	KEY ALT+V, ""
DEFINE PAD _41p113i4j OF _MSYSMENU PROMPT "Clientes" COLOR SCHEME 3 ;
	KEY ALT+C, ""
DEFINE PAD _41p113i4k OF _MSYSMENU PROMPT "Reportes" COLOR SCHEME 3 ;
	KEY ALT+R, ""
DEFINE PAD _41p113i4l OF _MSYSMENU PROMPT "N/C y N/D" COLOR SCHEME 3 ;
	KEY ALT+N, ""
DEFINE PAD _41p113i4m OF _MSYSMENU PROMPT "Impresión Documentos" COLOR SCHEME 3 ;
	KEY ALT+I, ""
DEFINE PAD _41p113i4n OF _MSYSMENU PROMPT "Documentos" COLOR SCHEME 3 ;
	KEY ALT+D, ""
DEFINE PAD _41p113i4o OF _MSYSMENU PROMPT "Salir" COLOR SCHEME 3 ;
	KEY ALT+S, ""
ON PAD _41p113i4i OF _MSYSMENU ACTIVATE POPUP ventas
ON PAD _41p113i4j OF _MSYSMENU ACTIVATE POPUP nuevoeleme
ON PAD _41p113i4k OF _MSYSMENU ACTIVATE POPUP reportes
ON PAD _41p113i4l OF _MSYSMENU ACTIVATE POPUP ncynd
ON PAD _41p113i4m OF _MSYSMENU ACTIVATE POPUP impresiónd
ON PAD _41p113i4n OF _MSYSMENU ACTIVATE POPUP documentos
ON PAD _41p113i4o OF _MSYSMENU ACTIVATE POPUP salir

DEFINE POPUP ventas MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF ventas PROMPT "Doc. Ventas"
DEFINE BAR 2 OF ventas PROMPT "Doc. Ventas Personal"
DEFINE BAR 3 OF ventas PROMPT "Cotizaciones"
DEFINE BAR 4 OF ventas PROMPT "\-"
DEFINE BAR 5 OF ventas PROMPT "Canje de Retenciones"
DEFINE BAR 6 OF ventas PROMPT "Anticipos Clientes"
DEFINE BAR 7 OF ventas PROMPT "Retenciones Recibidas"
DEFINE BAR 8 OF ventas PROMPT "\-"
DEFINE BAR 9 OF ventas PROMPT "liquidacions en Ventas"
DEFINE BAR 10 OF ventas PROMPT "\-"
DEFINE BAR 11 OF ventas PROMPT "Servicios"
DEFINE BAR 12 OF ventas PROMPT "Sujetos"
ON SELECTION BAR 1 OF ventas do form fac0101a
ON SELECTION BAR 2 OF ventas do form fac0102a
ON SELECTION BAR 3 OF ventas do form fac0103a
ON SELECTION BAR 5 OF ventas do form  fac0107
ON SELECTION BAR 6 OF ventas do form fac0104
ON SELECTION BAR 7 OF ventas do form retenreci
ON SELECTION BAR 9 OF ventas do form fac0113
ON SELECTION BAR 11 OF ventas do form adq0109
ON SELECTION BAR 12 OF ventas do form adm0303

DEFINE POPUP nuevoeleme MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF nuevoeleme PROMPT "Cheques Recibidos"
DEFINE BAR 2 OF nuevoeleme PROMPT "Confirmacion de Cheques"
DEFINE BAR 3 OF nuevoeleme PROMPT "Cheques otro Periodo"
DEFINE BAR 4 OF nuevoeleme PROMPT "\-"
DEFINE BAR 5 OF nuevoeleme PROMPT "Actualizar datos de Cheques"
DEFINE BAR 6 OF nuevoeleme PROMPT "\-"
DEFINE BAR 7 OF nuevoeleme PROMPT "Otros Creditos"
ON SELECTION BAR 1 OF nuevoeleme do form cheques
ON BAR 2 OF nuevoeleme ACTIVATE POPUP confirmaci
ON SELECTION BAR 3 OF nuevoeleme do form chequesind
ON SELECTION BAR 5 OF nuevoeleme do form actuche
ON SELECTION BAR 7 OF nuevoeleme do form cresuj

DEFINE POPUP confirmaci MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF confirmaci PROMPT "Individual"
DEFINE BAR 2 OF confirmaci PROMPT "Por fechas"
ON SELECTION BAR 1 OF confirmaci do form confirche
ON SELECTION BAR 2 OF confirmaci do form confirche2

DEFINE POPUP reportes MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF reportes PROMPT "Por Cliente"
DEFINE BAR 2 OF reportes PROMPT "\-"
DEFINE BAR 3 OF reportes PROMPT "Caja-Credito"
DEFINE BAR 4 OF reportes PROMPT "\-"
DEFINE BAR 5 OF reportes PROMPT "Por Items"
DEFINE BAR 6 OF reportes PROMPT "\-"
DEFINE BAR 7 OF reportes PROMPT "Por Transacción"
DEFINE BAR 8 OF reportes PROMPT "\-"
DEFINE BAR 9 OF reportes PROMPT "Ventas por Vendedor"
DEFINE BAR 10 OF reportes PROMPT "\-"
DEFINE BAR 11 OF reportes PROMPT "Configuracion de  Reportes"
ON BAR 1 OF reportes ACTIVATE POPUP porcliente
ON BAR 3 OF reportes ACTIVATE POPUP cajacredit
ON BAR 5 OF reportes ACTIVATE POPUP poritems
ON BAR 7 OF reportes ACTIVATE POPUP portransac
ON BAR 9 OF reportes ACTIVATE POPUP ventasporv
ON SELECTION BAR 11 OF reportes do form fac0308

DEFINE POPUP porcliente MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF porcliente PROMPT "Principales"
DEFINE BAR 2 OF porcliente PROMPT "Ventas"
DEFINE BAR 3 OF porcliente PROMPT "Historico"
DEFINE BAR 4 OF porcliente PROMPT "Estado de Cuenta"
DEFINE BAR 5 OF porcliente PROMPT "Cheques"
DEFINE BAR 6 OF porcliente PROMPT "Cheques NO Registrados"
DEFINE BAR 7 OF porcliente PROMPT "Cheques por Vecimiento"
DEFINE BAR 8 OF porcliente PROMPT "Ultimas Transacciones"
DEFINE BAR 9 OF porcliente PROMPT "Por Documento"
DEFINE BAR 10 OF porcliente PROMPT "Saldos con Intereses"
DEFINE BAR 11 OF porcliente PROMPT "Saldos Iniciales"
DEFINE BAR 12 OF porcliente PROMPT "Por Montos"
ON SELECTION BAR 1 OF porcliente do form impprin
ON SELECTION BAR 2 OF porcliente do form fac0301
ON SELECTION BAR 3 OF porcliente do form fac0304
ON BAR 4 OF porcliente ACTIVATE POPUP estadodecu
ON SELECTION BAR 5 OF porcliente do form fac0312
ON SELECTION BAR 6 OF porcliente do form fac0312a
ON SELECTION BAR 7 OF porcliente do form checliven
ON SELECTION BAR 8 OF porcliente do form fac0320
ON SELECTION BAR 9 OF porcliente do form fac0305
ON SELECTION BAR 10 OF porcliente do form fac0322
ON SELECTION BAR 11 OF porcliente do form fac0324
ON SELECTION BAR 12 OF porcliente do form fac0326

DEFINE POPUP estadodecu MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF estadodecu PROMPT "Normal"
DEFINE BAR 2 OF estadodecu PROMPT "Det Saldos Iniciales"
ON SELECTION BAR 1 OF estadodecu do form con0111
ON SELECTION BAR 2 OF estadodecu do form con0111a

DEFINE POPUP cajacredit MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF cajacredit PROMPT "Ingresos - Egresos (Contable)"
DEFINE BAR 2 OF cajacredit PROMPT "Ingresos-Egresos (det canc)"
DEFINE BAR 3 OF cajacredit PROMPT "Movimiento de la Cta Caja"
DEFINE BAR 4 OF cajacredit PROMPT "Detalle de Pagos Grupo"
DEFINE BAR 5 OF cajacredit PROMPT "Detalle  de Recaudacion Resumen diario"
DEFINE BAR 6 OF cajacredit PROMPT "Detalle de Pagos"
DEFINE BAR 7 OF cajacredit PROMPT "\-"
DEFINE BAR 8 OF cajacredit PROMPT "Pagos Descuadrados"
ON SELECTION BAR 1 OF cajacredit do form con0116a
ON SELECTION BAR 2 OF cajacredit do form fac0311
ON SELECTION BAR 3 OF cajacredit do form fac0311a
ON SELECTION BAR 4 OF cajacredit do form  fac0319
ON SELECTION BAR 5 OF cajacredit do form fac0311c
ON SELECTION BAR 6 OF cajacredit do form  fac0319a
ON SELECTION BAR 8 OF cajacredit do form fac0323

DEFINE POPUP poritems MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF poritems PROMPT "Documentos Por Items"
DEFINE BAR 2 OF poritems PROMPT "Ventas por Items"
DEFINE BAR 3 OF poritems PROMPT "Rentabilidad"
DEFINE BAR 4 OF poritems PROMPT "Articulos No vendidos"
ON SELECTION BAR 1 OF poritems do form fac0302
ON SELECTION BAR 2 OF poritems do form fac0315
ON SELECTION BAR 3 OF poritems do form fac0317
ON SELECTION BAR 4 OF poritems do form fac0327

DEFINE POPUP portransac MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF portransac PROMPT "Montos Totales"
DEFINE BAR 2 OF portransac PROMPT "Montos Totales (Grupos Doc)"
DEFINE BAR 3 OF portransac PROMPT "Montos Totales Recaudacion"
DEFINE BAR 4 OF portransac PROMPT "Montos Totales Recaudacion por Cta"
DEFINE BAR 5 OF portransac PROMPT "Montos Totales Recaudacion Varios Clientes"
DEFINE BAR 6 OF portransac PROMPT "Resumen Por Mes"
DEFINE BAR 7 OF portransac PROMPT "Retenciones Recibidas"
DEFINE BAR 8 OF portransac PROMPT "Terceros"
DEFINE BAR 9 OF portransac PROMPT "Por Vencimiento"
ON SELECTION BAR 1 OF portransac do form fac0303
ON SELECTION BAR 2 OF portransac do form fac0303a
ON SELECTION BAR 3 OF portransac do form fac0303c
ON SELECTION BAR 4 OF portransac do form fac0303e
ON SELECTION BAR 5 OF portransac do form fac0303f
ON SELECTION BAR 6 OF portransac do form fac0316
ON SELECTION BAR 7 OF portransac do form retenfac
ON SELECTION BAR 8 OF portransac do form rol0407
ON BAR 9 OF portransac ACTIVATE POPUP porfechade

DEFINE POPUP porfechade MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF porfechade PROMPT "Por Fechas"
DEFINE BAR 2 OF porfechade PROMPT "Varias Cuentas"
DEFINE BAR 3 OF porfechade PROMPT "Resumen"
DEFINE BAR 4 OF porfechade PROMPT "Vencidos y Por Vencer"
ON SELECTION BAR 1 OF porfechade do form fac0310
ON SELECTION BAR 2 OF porfechade do form fac0310a
ON SELECTION BAR 3 OF porfechade do form fac0310b
ON SELECTION BAR 4 OF porfechade do form fac0310c

DEFINE POPUP ventasporv MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF ventasporv PROMPT "Por Ventas"
DEFINE BAR 2 OF ventasporv PROMPT "Por Recaudacion"
DEFINE BAR 3 OF ventasporv PROMPT "\-"
DEFINE BAR 4 OF ventasporv PROMPT "Guias"
ON SELECTION BAR 1 OF ventasporv do form fac0309
ON SELECTION BAR 2 OF ventasporv do form fac0309a
ON SELECTION BAR 4 OF ventasporv do form inv0602b

DEFINE POPUP ncynd MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF ncynd PROMPT "Nota de Credito o Debito Clientes (Devoluciones)"
DEFINE BAR 2 OF ncynd PROMPT "Nota de Credito o Debito Clientes (Descuentos)"
DEFINE BAR 3 OF ncynd PROMPT "\-"
DEFINE BAR 4 OF ncynd PROMPT "Nota de Credito o Debito Clientes (Devoluciones Sin Ref.)"
DEFINE BAR 5 OF ncynd PROMPT "Nota de Credito o Debito Clientes (Descuentos Sin Ref.)"
ON SELECTION BAR 1 OF ncynd do form fac0401
ON SELECTION BAR 2 OF ncynd do form fac0402
ON SELECTION BAR 4 OF ncynd do form fac0401a
ON SELECTION BAR 5 OF ncynd do form fac0402a

DEFINE POPUP impresiónd MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF impresiónd PROMPT "Activar Impresión de Documentos"
DEFINE BAR 2 OF impresiónd PROMPT "Desactivar Impresión de Doc. Ventas"
ON SELECTION BAR 1 OF impresiónd do form actimpdoc
ON SELECTION BAR 2 OF impresiónd ;
	DO _41p113i4y ;
	IN LOCFILE("PROG\MENUFAC" ,"MPX;MPR|FXP;PRG" ,"DÓNDE está MENUFAC?")

DEFINE POPUP documentos MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF documentos PROMPT "Formas de Pago/Cobro"
DEFINE BAR 2 OF documentos PROMPT "Cancelación Saldos Iniciales"
DEFINE BAR 3 OF documentos PROMPT "Documento Bancario"
DEFINE BAR 4 OF documentos PROMPT "Contabilización"
DEFINE BAR 5 OF documentos PROMPT "\-"
DEFINE BAR 6 OF documentos PROMPT "Autorización"
DEFINE BAR 7 OF documentos PROMPT "Anulación"
DEFINE BAR 8 OF documentos PROMPT "\-"
DEFINE BAR 9 OF documentos PROMPT "Consulta"
DEFINE BAR 10 OF documentos PROMPT "\-"
DEFINE BAR 11 OF documentos PROMPT "Corrección"
DEFINE BAR 12 OF documentos PROMPT "Costos de Ventas"
ON SELECTION BAR 1 OF documentos do form adq0701b
ON SELECTION BAR 2 OF documentos do form saldoini
ON SELECTION BAR 3 OF documentos do form adq0702b
ON SELECTION BAR 4 OF documentos do form adq0703b
ON SELECTION BAR 6 OF documentos do form adq0707
ON SELECTION BAR 7 OF documentos do form adq0704
ON SELECTION BAR 9 OF documentos do form adq0705b
ON SELECTION BAR 11 OF documentos do form adq0706
ON SELECTION BAR 12 OF documentos do form costoven

DEFINE POPUP salir MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF salir PROMPT "Regresar al Menú Princ."
DEFINE BAR 2 OF salir PROMPT "Abandonar el Sistema"
ON SELECTION BAR 1 OF salir ;
	DO _41p113i4z ;
	IN LOCFILE("PROG\MENUFAC" ,"MPX;MPR|FXP;PRG" ,"DÓNDE está MENUFAC?")
ON SELECTION BAR 2 OF salir ;
	DO _41p113i50 ;
	IN LOCFILE("PROG\MENUFAC" ,"MPX;MPR|FXP;PRG" ,"DÓNDE está MENUFAC?")


*       *********************************************************
*       *                                                         
*       *              Limpiar Código y Procedimientos            
*       *                                                         
*       *********************************************************
*

read event

*       *********************************************************
*       *                                                         
*       * _41P113I4Y  ON SELECTION BAR 2 OF POPUP impresiónd      
*       *                                                         
*       * Procedure Origin:                                       
*       *                                                         
*       * From Menu:  MENUFAC.PRG,            Record:  114        
*       * Called By:  ON SELECTION BAR 2 OF POPUP impresiónd      
*       * Prompt:     Desactivar Impresión de Doc. Ventas         
*       * Snippet:    1                                           
*       *                                                         
*       *********************************************************
*
PROCEDURE _41p113i4y
if type('_screen.objimp')='O'
	_screen.removeobject('objimp')
endif


*       *********************************************************
*       *                                                         
*       * _41P113I4Z  ON SELECTION BAR 1 OF POPUP salir           
*       *                                                         
*       * Procedure Origin:                                       
*       *                                                         
*       * From Menu:  MENUFAC.PRG,            Record:  132        
*       * Called By:  ON SELECTION BAR 1 OF POPUP salir           
*       * Prompt:     Regresar al Menú Princ.                     
*       * Snippet:    2                                           
*       *                                                         
*       *********************************************************
*
PROCEDURE _41p113i4z
do while type('_screen.activeform')='O'
	_screen.activeform.release
enddo	

if used('ACCESOS') then
	sele accesos
	use
endif

deactivate menu _msysmenu
clea even
clea read
return


*       *********************************************************
*       *                                                         
*       * _41P113I50  ON SELECTION BAR 2 OF POPUP salir           
*       *                                                         
*       * Procedure Origin:                                       
*       *                                                         
*       * From Menu:  MENUFAC.PRG,            Record:  133        
*       * Called By:  ON SELECTION BAR 2 OF POPUP salir           
*       * Prompt:     Abandonar el Sistema                        
*       * Snippet:    3                                           
*       *                                                         
*       *********************************************************
*
PROCEDURE _41p113i50
deactivate menu _msysmenu
clea even
clea read
final=1
return


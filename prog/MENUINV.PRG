*       *********************************************************
*       *                                                         
*       * 13/02/2016            MENUINV.PRG             17:53:06  
*       *                                                         
*       *********************************************************
*       *                                                         
*       * Nombre del autor                                        
*       *                                                         
*       * Copyright (C) 2016 Nombre de compañía                   
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

DEFINE PAD _4kc12c0l3 OF _MSYSMENU PROMPT "Opc. Generales" COLOR SCHEME 3 ;
	KEY ALT+O, ""
DEFINE PAD _4kc12c0l4 OF _MSYSMENU PROMPT "Producción" COLOR SCHEME 3 ;
	KEY ALT+P, ""
DEFINE PAD _4kc12c0l5 OF _MSYSMENU PROMPT "Ingresos-Egresos" COLOR SCHEME 3 ;
	KEY ALT+I, ""
DEFINE PAD _4kc12c0l6 OF _MSYSMENU PROMPT "Ajustes" COLOR SCHEME 3 ;
	KEY ALT+A, ""
DEFINE PAD _4kc12c0l7 OF _MSYSMENU PROMPT "Reportes" COLOR SCHEME 3 ;
	KEY ALT+R, ""
DEFINE PAD _4kc12c0l8 OF _MSYSMENU PROMPT "Entrega Ventas" COLOR SCHEME 3 ;
	KEY ALT+E, ""
DEFINE PAD _4kc12c0l9 OF _MSYSMENU PROMPT "Documentos" COLOR SCHEME 3 ;
	KEY ALT+D, ""
DEFINE PAD _4kc12c0la OF _MSYSMENU PROMPT "Impresión Documentos" COLOR SCHEME 3 ;
	KEY ALT+I, ""
DEFINE PAD _4kc12c0lb OF _MSYSMENU PROMPT "Salir" COLOR SCHEME 3 ;
	KEY ALT+S, ""
ON PAD _4kc12c0l3 OF _MSYSMENU ACTIVATE POPUP opcgeneral
ON PAD _4kc12c0l4 OF _MSYSMENU ACTIVATE POPUP _1w70rn3yl
ON PAD _4kc12c0l5 OF _MSYSMENU ACTIVATE POPUP ingresoseg
ON PAD _4kc12c0l6 OF _MSYSMENU ACTIVATE POPUP ajustes
ON PAD _4kc12c0l7 OF _MSYSMENU ACTIVATE POPUP reportes
ON PAD _4kc12c0l8 OF _MSYSMENU ACTIVATE POPUP _29217hc9u
ON PAD _4kc12c0l9 OF _MSYSMENU ACTIVATE POPUP nuevoeleme
ON PAD _4kc12c0la OF _MSYSMENU ACTIVATE POPUP _1wy1d6g9y
ON PAD _4kc12c0lb OF _MSYSMENU ACTIVATE POPUP salir

DEFINE POPUP opcgeneral MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF opcgeneral PROMPT "Mantenimiento del Inventario"
DEFINE BAR 2 OF opcgeneral PROMPT "Conversiones"
DEFINE BAR 3 OF opcgeneral PROMPT "\-"
DEFINE BAR 4 OF opcgeneral PROMPT "listas de Precios"
ON SELECTION BAR 1 OF opcgeneral do form inv0101
ON SELECTION BAR 2 OF opcgeneral do form inv0108
ON BAR 4 OF opcgeneral ACTIVATE POPUP listasdepr

DEFINE POPUP listasdepr MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF listasdepr PROMPT "Por Lista"
DEFINE BAR 2 OF listasdepr PROMPT "5 Listas"
DEFINE BAR 3 OF listasdepr PROMPT "Precios"
DEFINE BAR 4 OF listasdepr PROMPT "Ingreso Individual"
ON SELECTION BAR 1 OF listasdepr do form inv0109
ON SELECTION BAR 2 OF listasdepr do form inv0109a
ON SELECTION BAR 3 OF listasdepr do form inv0109d
ON SELECTION BAR 4 OF listasdepr do form inv0109e

DEFINE POPUP _1w70rn3yl MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF _1w70rn3yl PROMPT "Componentes"
DEFINE BAR 2 OF _1w70rn3yl PROMPT "\-"
DEFINE BAR 3 OF _1w70rn3yl PROMPT "Orden de Producción"
DEFINE BAR 4 OF _1w70rn3yl PROMPT "\-"
DEFINE BAR 5 OF _1w70rn3yl PROMPT "Consulta de Ordenes"
DEFINE BAR 6 OF _1w70rn3yl PROMPT "Reporte de Ordenes"
DEFINE BAR 7 OF _1w70rn3yl PROMPT "Anulacion de Ordenes de Producción"
DEFINE BAR 8 OF _1w70rn3yl PROMPT "\-"
DEFINE BAR 9 OF _1w70rn3yl PROMPT "Generacion de Documentos"
ON SELECTION BAR 1 OF _1w70rn3yl do form inv0111
ON SELECTION BAR 3 OF _1w70rn3yl do form inv0112
ON SELECTION BAR 5 OF _1w70rn3yl do form inv0113
ON SELECTION BAR 6 OF _1w70rn3yl do form inv0116
ON SELECTION BAR 7 OF _1w70rn3yl do form inv0114
ON SELECTION BAR 9 OF _1w70rn3yl do form inv0115

DEFINE POPUP ingresoseg MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF ingresoseg PROMPT "Ingresos-Egresos"
DEFINE BAR 2 OF ingresoseg PROMPT "Segun Doc. Inicial"
DEFINE BAR 3 OF ingresoseg PROMPT "Despacho de Pedidos"
DEFINE BAR 4 OF ingresoseg PROMPT "\-"
DEFINE BAR 5 OF ingresoseg PROMPT "Ingresos-Egresos por Bodega"
ON SELECTION BAR 1 OF ingresoseg do form inv0401
ON SELECTION BAR 2 OF ingresoseg do form inv0401a
ON SELECTION BAR 3 OF ingresoseg do form inv0403
ON SELECTION BAR 5 OF ingresoseg do form inv0404

DEFINE POPUP ajustes MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF ajustes PROMPT "Sados Iniciales"
DEFINE BAR 2 OF ajustes PROMPT "\-"
DEFINE BAR 3 OF ajustes PROMPT "Costos y Saldos de Stock General"
DEFINE BAR 4 OF ajustes PROMPT "Calculo de Costos en doc Ventas"
DEFINE BAR 5 OF ajustes PROMPT "\-"
DEFINE BAR 6 OF ajustes PROMPT "Ultima fecha de costeo"
DEFINE BAR 7 OF ajustes PROMPT "Cargo o Descargo de la Cta Inventario"
DEFINE BAR 8 OF ajustes PROMPT "\-"
DEFINE BAR 9 OF ajustes PROMPT "Conteo Fisico"
DEFINE BAR 10 OF ajustes PROMPT "Generacion de Saldos Mensuales"
DEFINE BAR 11 OF ajustes PROMPT "Generacion de Saldos Varios meses"
DEFINE BAR 12 OF ajustes PROMPT "Actualizacion de Costos Proveedor"
DEFINE BAR 13 OF ajustes PROMPT "\-"
DEFINE BAR 14 OF ajustes PROMPT "Registro de Desc. por N/C"
ON BAR 1 OF ajustes ACTIVATE POPUP sadosinici
ON SELECTION BAR 3 OF ajustes do form inv0207
ON SELECTION BAR 4 OF ajustes do form inv0208
ON SELECTION BAR 6 OF ajustes do form inv0407
ON SELECTION BAR 7 OF ajustes do form inv0209a
ON SELECTION BAR 9 OF ajustes do form inv0211
ON SELECTION BAR 10 OF ajustes do form inv0212a
ON SELECTION BAR 11 OF ajustes do form inv0212b
ON SELECTION BAR 12 OF ajustes do form inv0213
ON SELECTION BAR 14 OF ajustes do form inv0216

DEFINE POPUP sadosinici MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF sadosinici PROMPT "Generar saldos"
DEFINE BAR 2 OF sadosinici PROMPT "Importar saldos"
DEFINE BAR 3 OF sadosinici PROMPT "\-"
DEFINE BAR 4 OF sadosinici PROMPT "Edición de Item"
DEFINE BAR 5 OF sadosinici PROMPT "\-"
DEFINE BAR 6 OF sadosinici PROMPT "Contabilización"
DEFINE BAR 7 OF sadosinici PROMPT "Unificar saldos Contables y Kardex"
ON SELECTION BAR 1 OF sadosinici do form inv0205
ON SELECTION BAR 2 OF sadosinici do form inv0205a
ON SELECTION BAR 4 OF sadosinici do form inv0206
ON SELECTION BAR 6 OF sadosinici do form inv0210
ON SELECTION BAR 7 OF sadosinici do form inv0205c

DEFINE POPUP reportes MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF reportes PROMPT "Movimiento Historico de Items"
DEFINE BAR 2 OF reportes PROMPT "Movimiento Historico de Items por Sujetos"
DEFINE BAR 3 OF reportes PROMPT "Resumen Por Tipo"
DEFINE BAR 4 OF reportes PROMPT "Resumen Items por Sujetos"
DEFINE BAR 5 OF reportes PROMPT "\-"
DEFINE BAR 6 OF reportes PROMPT "Egresos de Bodega"
DEFINE BAR 7 OF reportes PROMPT "Saldos Iniciales"
DEFINE BAR 8 OF reportes PROMPT "Por fechas de Vencimiento"
DEFINE BAR 9 OF reportes PROMPT "Lista de Precios"
DEFINE BAR 10 OF reportes PROMPT "Lista de Items"
DEFINE BAR 11 OF reportes PROMPT "Precios Modificados"
DEFINE BAR 12 OF reportes PROMPT "Montos Totales (Grupos Doc)"
DEFINE BAR 13 OF reportes PROMPT "Doc. Generados y No Generados"
DEFINE BAR 14 OF reportes PROMPT "Doc. Sin Entrega de Articulos"
DEFINE BAR 15 OF reportes PROMPT "Listado Conteo Fisico"
DEFINE BAR 16 OF reportes PROMPT "Detalle de Items por documento"
DEFINE BAR 17 OF reportes PROMPT "Movimiento Historico (Anulados)"
DEFINE BAR 18 OF reportes PROMPT "Items sin Costo en Doc."
DEFINE BAR 19 OF reportes PROMPT "Etiquetas"
DEFINE BAR 20 OF reportes PROMPT "Etiquetas Cod de Barra"
DEFINE BAR 21 OF reportes PROMPT "Comparacion Kardex-Contabilidad"
ON SELECTION BAR 1 OF reportes do form inv0601c
ON SELECTION BAR 2 OF reportes do form inv0602
ON SELECTION BAR 3 OF reportes do form inv0603
ON SELECTION BAR 4 OF reportes do form inv0602a
ON SELECTION BAR 6 OF reportes do form inv0611
ON SELECTION BAR 7 OF reportes DO FORM IMPSALITEM
ON SELECTION BAR 8 OF reportes do form inv0610
ON SELECTION BAR 9 OF reportes do form listapre
ON SELECTION BAR 10 OF reportes do form impitem
ON SELECTION BAR 11 OF reportes do form listapre2
ON SELECTION BAR 12 OF reportes do form fac0303a
ON SELECTION BAR 13 OF reportes do form inv0606
ON SELECTION BAR 14 OF reportes do form inv0607
ON SELECTION BAR 15 OF reportes do form inv0608
ON SELECTION BAR 16 OF reportes do form inv0609
ON SELECTION BAR 17 OF reportes do form inv0601a
ON SELECTION BAR 18 OF reportes do form inv0613
ON SELECTION BAR 19 OF reportes do form impeti
ON SELECTION BAR 20 OF reportes do form impeti2
ON SELECTION BAR 21 OF reportes do form inv0601b

DEFINE POPUP _29217hc9u MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF _29217hc9u PROMPT "Entrega Productos Contado"
DEFINE BAR 2 OF _29217hc9u PROMPT "Entrega Productos Credito"
DEFINE BAR 3 OF _29217hc9u PROMPT "\-"
DEFINE BAR 4 OF _29217hc9u PROMPT "Confirmaión por Grupo"
ON SELECTION BAR 1 OF _29217hc9u do form inv0701
ON SELECTION BAR 2 OF _29217hc9u do form inv0701b
ON SELECTION BAR 4 OF _29217hc9u do form inv0701c

DEFINE POPUP nuevoeleme MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF nuevoeleme PROMPT "Desbloqueo"
DEFINE BAR 2 OF nuevoeleme PROMPT "Autorización"
DEFINE BAR 3 OF nuevoeleme PROMPT "\-"
DEFINE BAR 4 OF nuevoeleme PROMPT "Cancelación"
DEFINE BAR 5 OF nuevoeleme PROMPT "Documento Bancario"
DEFINE BAR 6 OF nuevoeleme PROMPT "Contabilización"
DEFINE BAR 7 OF nuevoeleme PROMPT "\-"
DEFINE BAR 8 OF nuevoeleme PROMPT "Autorización"
DEFINE BAR 9 OF nuevoeleme PROMPT "Anulación"
DEFINE BAR 10 OF nuevoeleme PROMPT "\-"
DEFINE BAR 11 OF nuevoeleme PROMPT "Consulta"
DEFINE BAR 12 OF nuevoeleme PROMPT "\-"
DEFINE BAR 13 OF nuevoeleme PROMPT "Modificar Items"
ON SELECTION BAR 1 OF nuevoeleme do form adq0708
ON SELECTION BAR 2 OF nuevoeleme do form adq0707
ON SELECTION BAR 4 OF nuevoeleme do form adq0701b
ON SELECTION BAR 5 OF nuevoeleme do form adq0702b
ON SELECTION BAR 6 OF nuevoeleme do form adq0703b
ON SELECTION BAR 8 OF nuevoeleme do form adq0707
ON SELECTION BAR 9 OF nuevoeleme do form adq0704
ON SELECTION BAR 11 OF nuevoeleme do form adq0705b
ON SELECTION BAR 13 OF nuevoeleme do form moditems

DEFINE POPUP _1wy1d6g9y MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF _1wy1d6g9y PROMPT "Activar Impresión de Documentos"
DEFINE BAR 2 OF _1wy1d6g9y PROMPT "Desactivar Impresión"
ON SELECTION BAR 1 OF _1wy1d6g9y do form actimpdoc
ON SELECTION BAR 2 OF _1wy1d6g9y ;
	DO _4kc12c0lx ;
	IN LOCFILE("PROG\MENUINV" ,"MPX;MPR|FXP;PRG" ,"DÓNDE está MENUINV?")

DEFINE POPUP salir MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF salir PROMPT "Regresar al Menú Princ."
DEFINE BAR 2 OF salir PROMPT "Abandonar el Sistema"
ON SELECTION BAR 1 OF salir ;
	DO _4kc12c0lz ;
	IN LOCFILE("PROG\MENUINV" ,"MPX;MPR|FXP;PRG" ,"DÓNDE está MENUINV?")
ON SELECTION BAR 2 OF salir ;
	DO _4kc12c0m1 ;
	IN LOCFILE("PROG\MENUINV" ,"MPX;MPR|FXP;PRG" ,"DÓNDE está MENUINV?")


*       *********************************************************
*       *                                                         
*       *              Limpiar Código y Procedimientos            
*       *                                                         
*       *********************************************************
*

read event

*       *********************************************************
*       *                                                         
*       * _4KC12C0LX  ON SELECTION BAR 2 OF POPUP _1wy1d6g9y      
*       *                                                         
*       * Procedure Origin:                                       
*       *                                                         
*       * From Menu:  MENUINV.PRG,            Record:  117        
*       * Called By:  ON SELECTION BAR 2 OF POPUP _1wy1d6g9y      
*       * Prompt:     Desactivar Impresión                        
*       * Snippet:    1                                           
*       *                                                         
*       *********************************************************
*
PROCEDURE _4kc12c0lx
if type('_screen.objimp')='O'
	_screen.removeobject('objimp')
endif


*       *********************************************************
*       *                                                         
*       * _4KC12C0LZ  ON SELECTION BAR 1 OF POPUP salir           
*       *                                                         
*       * Procedure Origin:                                       
*       *                                                         
*       * From Menu:  MENUINV.PRG,            Record:  120        
*       * Called By:  ON SELECTION BAR 1 OF POPUP salir           
*       * Prompt:     Regresar al Menú Princ.                     
*       * Snippet:    2                                           
*       *                                                         
*       *********************************************************
*
PROCEDURE _4kc12c0lz
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
*       * _4KC12C0M1  ON SELECTION BAR 2 OF POPUP salir           
*       *                                                         
*       * Procedure Origin:                                       
*       *                                                         
*       * From Menu:  MENUINV.PRG,            Record:  121        
*       * Called By:  ON SELECTION BAR 2 OF POPUP salir           
*       * Prompt:     Abandonar el Sistema                        
*       * Snippet:    3                                           
*       *                                                         
*       *********************************************************
*
PROCEDURE _4kc12c0m1
deactivate menu _msysmenu
clea even
clea read
final=1
return


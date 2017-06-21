*       *********************************************************
*       *                                                         
*       * 11/10/2010            MENUCAJ.PRG             22:55:12  
*       *                                                         
*       *********************************************************
*       *                                                         
*       * Nombre del autor                                        
*       *                                                         
*       * Copyright (C) 2010 Nombre de compañía                   
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

DEFINE PAD _31u1d4iuh OF _MSYSMENU PROMPT "Ventas" COLOR SCHEME 3 ;
	KEY ALT+V, ""
DEFINE PAD _31u1d4iui OF _MSYSMENU PROMPT "Reportes" COLOR SCHEME 3 ;
	KEY ALT+R, ""
DEFINE PAD _31u1d4iuj OF _MSYSMENU PROMPT "Configuracion" COLOR SCHEME 3 ;
	KEY ALT+C, ""
DEFINE PAD _31u1d4iuk OF _MSYSMENU PROMPT "Documentos" COLOR SCHEME 3 ;
	KEY ALT+D, ""
DEFINE PAD _31u1d4iul OF _MSYSMENU PROMPT "Impresion" COLOR SCHEME 3 ;
	KEY ALT+I, ""
DEFINE PAD _31u1d4ium OF _MSYSMENU PROMPT "Salir" COLOR SCHEME 3 ;
	KEY ALT+S, ""
ON PAD _31u1d4iuh OF _MSYSMENU ACTIVATE POPUP ventas
ON PAD _31u1d4iui OF _MSYSMENU ACTIVATE POPUP reportes
ON PAD _31u1d4iuj OF _MSYSMENU ACTIVATE POPUP configurac
ON PAD _31u1d4iuk OF _MSYSMENU ACTIVATE POPUP documentos
ON PAD _31u1d4iul OF _MSYSMENU ACTIVATE POPUP _2u51edjuu
ON SELECTION PAD _31u1d4ium OF _MSYSMENU ;
	DO _31u1d4iun ;
	IN LOCFILE("PROG\MENUCAJ" ,"MPX;MPR|FXP;PRG" ,"DÓNDE está MENUCAJ?")

DEFINE POPUP ventas MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF ventas PROMPT "Doc. Ventas"
DEFINE BAR 2 OF ventas PROMPT "Cotizaciones"
DEFINE BAR 3 OF ventas PROMPT "Dco Generados"
DEFINE BAR 4 OF ventas PROMPT "\-"
DEFINE BAR 5 OF ventas PROMPT "Devoluciones"
DEFINE BAR 6 OF ventas PROMPT "\-"
DEFINE BAR 7 OF ventas PROMPT "Cheques Ventas"
DEFINE BAR 8 OF ventas PROMPT "\-"
DEFINE BAR 9 OF ventas PROMPT "Consulta de Items"
ON SELECTION BAR 1 OF ventas do form fac0101
ON SELECTION BAR 2 OF ventas do form fac0103
ON SELECTION BAR 3 OF ventas do form fac0101b
ON SELECTION BAR 5 OF ventas do form fac0401
ON SELECTION BAR 7 OF ventas do form  cheques
ON SELECTION BAR 9 OF ventas do form fac0109a

DEFINE POPUP reportes MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF reportes PROMPT "Ingresos - Egresos (Contable)"
DEFINE BAR 2 OF reportes PROMPT "Ingresos - Egresos (det cancelacion)"
DEFINE BAR 3 OF reportes PROMPT "Movimiento Cta. Caja"
DEFINE BAR 4 OF reportes PROMPT "Doc. No Contabilizados"
DEFINE BAR 5 OF reportes PROMPT "Montos Totales"
DEFINE BAR 6 OF reportes PROMPT "Cheques Clientes"
ON SELECTION BAR 1 OF reportes do form con0116a
ON SELECTION BAR 2 OF reportes do form fac0311
ON SELECTION BAR 3 OF reportes do form fac0311a
ON SELECTION BAR 4 OF reportes do form con0115
ON SELECTION BAR 5 OF reportes do form fac0303
ON SELECTION BAR 6 OF reportes do form fac0312

DEFINE POPUP configurac MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF configurac PROMPT "Apariencia"
DEFINE BAR 2 OF configurac PROMPT "Definir Cajero"
DEFINE BAR 3 OF configurac PROMPT "\-"
DEFINE BAR 4 OF configurac PROMPT "Consulta"
ON SELECTION BAR 1 OF configurac do form set0403
ON SELECTION BAR 2 OF configurac do form set0404
ON SELECTION BAR 4 OF configurac do form adq0705b

DEFINE POPUP documentos MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF documentos PROMPT "Consulta"
ON SELECTION BAR 1 OF documentos do form adq0705b

DEFINE POPUP _2u51edjuu MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF _2u51edjuu PROMPT "Activar Impresión de Documentos"
DEFINE BAR 2 OF _2u51edjuu PROMPT "Desactivar Impresión de Doc. Ventas"
ON SELECTION BAR 1 OF _2u51edjuu do form actimpdoc
ON SELECTION BAR 2 OF _2u51edjuu ;
	DO _31u1d4iuo ;
	IN LOCFILE("PROG\MENUCAJ" ,"MPX;MPR|FXP;PRG" ,"DÓNDE está MENUCAJ?")


*       *********************************************************
*       *                                                         
*       *              Limpiar Código y Procedimientos            
*       *                                                         
*       *********************************************************
*

read event

*       *********************************************************
*       *                                                         
*       * _31U1D4IUN  ON SELECTION PAD                            
*       *                                                         
*       * Procedure Origin:                                       
*       *                                                         
*       * From Menu:  MENUCAJ.PRG,            Record:   39        
*       * Called By:  ON SELECTION PAD                            
*       * Prompt:     Salir                                       
*       * Snippet:    1                                           
*       *                                                         
*       *********************************************************
*
PROCEDURE _31u1d4iun
local a
a=.f.
do form claveadm to a
if !a
	return
endif

deactivate menu _msysmenu
clea even
clea read
final=1
return



*       *********************************************************
*       *                                                         
*       * _31U1D4IUO  ON SELECTION BAR 2 OF POPUP _2u51edjuu      
*       *                                                         
*       * Procedure Origin:                                       
*       *                                                         
*       * From Menu:  MENUCAJ.PRG,            Record:   38        
*       * Called By:  ON SELECTION BAR 2 OF POPUP _2u51edjuu      
*       * Prompt:     Desactivar Impresión de Doc. Ventas         
*       * Snippet:    2                                           
*       *                                                         
*       *********************************************************
*
PROCEDURE _31u1d4iuo
if type('_screen.objimp')='O'
	_screen.removeobject('objimp')
endif

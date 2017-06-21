*       *********************************************************
*       *                                                         
*       * 13/01/2009            MENUACT.PRG             00:27:40  
*       *                                                         
*       *********************************************************
*       *                                                         
*       * Nombre del autor                                        
*       *                                                         
*       * Copyright (C) 2009 Nombre de compañía                   
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

DEFINE PAD _2k400zlds OF _MSYSMENU PROMPT "Opciones Generales" COLOR SCHEME 3 ;
	KEY ALT+O, ""
DEFINE PAD _2k400zldt OF _MSYSMENU PROMPT "Reportes" COLOR SCHEME 3 ;
	KEY ALT+R, ""
DEFINE PAD _2k400zldu OF _MSYSMENU PROMPT "Documentos" COLOR SCHEME 3 ;
	KEY ALT+D, ""
DEFINE PAD _2k400zldv OF _MSYSMENU PROMPT "Salir" COLOR SCHEME 3 ;
	KEY ALT+S, ""
ON PAD _2k400zlds OF _MSYSMENU ACTIVATE POPUP opcionesge
ON PAD _2k400zldt OF _MSYSMENU ACTIVATE POPUP reportes
ON PAD _2k400zldu OF _MSYSMENU ACTIVATE POPUP documentos
ON PAD _2k400zldv OF _MSYSMENU ACTIVATE POPUP salir

DEFINE POPUP opcionesge MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF opcionesge PROMPT "Registro de Activo"
DEFINE BAR 2 OF opcionesge PROMPT "Aumentos de Activo"
DEFINE BAR 3 OF opcionesge PROMPT "Bajas del Activo"
DEFINE BAR 4 OF opcionesge PROMPT "Disminución de Activos"
DEFINE BAR 5 OF opcionesge PROMPT "\-"
DEFINE BAR 6 OF opcionesge PROMPT "Depreciación"
DEFINE BAR 7 OF opcionesge PROMPT "\-"
DEFINE BAR 8 OF opcionesge PROMPT "Pasar Saldos"
DEFINE BAR 9 OF opcionesge PROMPT "Importar Saldos"
DEFINE BAR 10 OF opcionesge PROMPT "\-"
DEFINE BAR 11 OF opcionesge PROMPT "Venta de Activos"
DEFINE BAR 12 OF opcionesge PROMPT "Edicion de Activos"
ON SELECTION BAR 1 OF opcionesge do form act0101
ON SELECTION BAR 2 OF opcionesge do form act0102
ON SELECTION BAR 3 OF opcionesge do form act0206
ON SELECTION BAR 4 OF opcionesge do form act0207
ON BAR 6 OF opcionesge ACTIVATE POPUP depreciaci
ON SELECTION BAR 11 OF opcionesge do form act0108
ON SELECTION BAR 12 OF opcionesge do form act0110

DEFINE POPUP depreciaci MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF depreciaci PROMPT "Calculo"
DEFINE BAR 2 OF depreciaci PROMPT "Reporte Historico"
DEFINE BAR 3 OF depreciaci PROMPT "Registro"
DEFINE BAR 4 OF depreciaci PROMPT "Contabilización"
ON SELECTION BAR 1 OF depreciaci do form act0104a
ON SELECTION BAR 2 OF depreciaci do form act0104b
ON SELECTION BAR 3 OF depreciaci do form act0104c
ON SELECTION BAR 4 OF depreciaci do form act0212

DEFINE POPUP reportes MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF reportes PROMPT "Por Persona"
DEFINE BAR 2 OF reportes PROMPT "Por Persona Total"
DEFINE BAR 3 OF reportes PROMPT "Por Tipo de Clasificación"
DEFINE BAR 4 OF reportes PROMPT "Activos y Bienes de Control"
DEFINE BAR 5 OF reportes PROMPT "Dados de Baja por Persona"
DEFINE BAR 6 OF reportes PROMPT "Depreciación por Fecha"
DEFINE BAR 7 OF reportes PROMPT "Secuencial de Documento"
DEFINE BAR 8 OF reportes PROMPT "Autofabricados"
DEFINE BAR 9 OF reportes PROMPT "Por Responsable y Estado"
DEFINE BAR 10 OF reportes PROMPT "Por Fechas de Adquisión"
DEFINE BAR 11 OF reportes PROMPT "Por Grupo de Activo"
DEFINE BAR 12 OF reportes PROMPT "Repuestos por Activo"
DEFINE BAR 13 OF reportes PROMPT "Acta de Baja de activo"
ON SELECTION BAR 1 OF reportes do form act0209
ON SELECTION BAR 3 OF reportes do form act0210
ON SELECTION BAR 5 OF reportes do form act0211
ON SELECTION BAR 6 OF reportes do form act0104b
ON SELECTION BAR 8 OF reportes do form act0202
ON SELECTION BAR 9 OF reportes do form act0203
ON SELECTION BAR 10 OF reportes do form act0204
ON SELECTION BAR 11 OF reportes do form act0205
ON SELECTION BAR 12 OF reportes do form act0213
ON SELECTION BAR 13 OF reportes Do Form act0208

DEFINE POPUP documentos MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF documentos PROMPT "Consulta"
DEFINE BAR 2 OF documentos PROMPT "\-"
DEFINE BAR 3 OF documentos PROMPT "Formas de Pago/Cobro"
DEFINE BAR 4 OF documentos PROMPT "\-"
DEFINE BAR 5 OF documentos PROMPT "Contabilización"
DEFINE BAR 6 OF documentos PROMPT "\-"
DEFINE BAR 7 OF documentos PROMPT "Anulación"
ON SELECTION BAR 1 OF documentos do form adq0705b
ON SELECTION BAR 3 OF documentos do form adq0701b
ON SELECTION BAR 5 OF documentos do form adq0703b
ON SELECTION BAR 7 OF documentos do form adq0704

DEFINE POPUP salir MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF salir PROMPT "Regresar al Menu Princ."
DEFINE BAR 2 OF salir PROMPT "Abandonar el sistema"
ON SELECTION BAR 1 OF salir ;
	DO _2k400zldw ;
	IN LOCFILE("PROG\MENUACT" ,"MPX;MPR|FXP;PRG" ,"DÓNDE está MENUACT?")
ON SELECTION BAR 2 OF salir ;
	DO _2k400zldx ;
	IN LOCFILE("PROG\MENUACT" ,"MPX;MPR|FXP;PRG" ,"DÓNDE está MENUACT?")


*       *********************************************************
*       *                                                         
*       *              Limpiar Código y Procedimientos            
*       *                                                         
*       *********************************************************
*

read event

*       *********************************************************
*       *                                                         
*       * _2K400ZLDW  ON SELECTION BAR 1 OF POPUP salir           
*       *                                                         
*       * Procedure Origin:                                       
*       *                                                         
*       * From Menu:  MENUACT.PRG,            Record:   53        
*       * Called By:  ON SELECTION BAR 1 OF POPUP salir           
*       * Prompt:     Regresar al Menu Princ.                     
*       * Snippet:    1                                           
*       *                                                         
*       *********************************************************
*
PROCEDURE _2k400zldw
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
*       * _2K400ZLDX  ON SELECTION BAR 2 OF POPUP salir           
*       *                                                         
*       * Procedure Origin:                                       
*       *                                                         
*       * From Menu:  MENUACT.PRG,            Record:   54        
*       * Called By:  ON SELECTION BAR 2 OF POPUP salir           
*       * Prompt:     Abandonar el sistema                        
*       * Snippet:    2                                           
*       *                                                         
*       *********************************************************
*
PROCEDURE _2k400zldx
deactivate menu _msysmenu
clea even
clea read
final=1
return


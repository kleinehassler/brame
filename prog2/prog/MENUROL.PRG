*       *********************************************************
*       *                                                         
*       * 26/03/2009            MENUROL.PRG             16:21:46  
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

DEFINE PAD _2m40z2khh OF _MSYSMENU PROMPT "Ingreso de Datos" COLOR SCHEME 3 ;
	KEY ALT+I, ""
DEFINE PAD _2m40z2khi OF _MSYSMENU PROMPT "Registro Rol" COLOR SCHEME 3 ;
	KEY ALT+R, ""
DEFINE PAD _2m40z2khj OF _MSYSMENU PROMPT "Reportes" COLOR SCHEME 3 ;
	KEY ALT+R, ""
DEFINE PAD _2m40z2khk OF _MSYSMENU PROMPT "Documentos" COLOR SCHEME 3 ;
	KEY ALT+D, ""
DEFINE PAD _2m40z2khl OF _MSYSMENU PROMPT "Configuracion" COLOR SCHEME 3 ;
	KEY ALT+C, ""
DEFINE PAD _2m40z2khm OF _MSYSMENU PROMPT "Salir" COLOR SCHEME 3 ;
	KEY ALT+S, ""
ON PAD _2m40z2khh OF _MSYSMENU ACTIVATE POPUP ingresoded
ON PAD _2m40z2khi OF _MSYSMENU ACTIVATE POPUP registroro
ON PAD _2m40z2khj OF _MSYSMENU ACTIVATE POPUP reportes
ON PAD _2m40z2khk OF _MSYSMENU ACTIVATE POPUP documentos
ON PAD _2m40z2khl OF _MSYSMENU ACTIVATE POPUP configurac
ON PAD _2m40z2khm OF _MSYSMENU ACTIVATE POPUP salir

DEFINE POPUP ingresoded MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF ingresoded PROMPT "Creación de Personas"
DEFINE BAR 2 OF ingresoded PROMPT "\-"
DEFINE BAR 3 OF ingresoded PROMPT "Generar Rol"
DEFINE BAR 4 OF ingresoded PROMPT "\-"
DEFINE BAR 5 OF ingresoded PROMPT "Rol 2da Forma"
DEFINE BAR 6 OF ingresoded PROMPT "\-"
DEFINE BAR 7 OF ingresoded PROMPT "Liquidacion de Sueldo"
ON BAR 1 OF ingresoded ACTIVATE POPUP creaciónde
ON SELECTION BAR 3 OF ingresoded do form rol0104
ON SELECTION BAR 5 OF ingresoded do form rol0108
ON SELECTION BAR 7 OF ingresoded do form rol0107

DEFINE POPUP creaciónde MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF creaciónde PROMPT "Datos Generales Sujetos"
DEFINE BAR 2 OF creaciónde PROMPT "Agregar Datos de Empleados"
ON SELECTION BAR 1 OF creaciónde do form adm0303
ON SELECTION BAR 2 OF creaciónde do form rol0101b

DEFINE POPUP registroro MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF registroro PROMPT "Anticipos/Haberes"
DEFINE BAR 2 OF registroro PROMPT "Quincena - Decimos"
DEFINE BAR 3 OF registroro PROMPT "\-"
DEFINE BAR 4 OF registroro PROMPT "Multas/Descuentos"
DEFINE BAR 5 OF registroro PROMPT "\-"
DEFINE BAR 6 OF registroro PROMPT "Asistencia y Horas Extras"
DEFINE BAR 7 OF registroro PROMPT "Feriados"
DEFINE BAR 8 OF registroro PROMPT "Alimentación"
DEFINE BAR 9 OF registroro PROMPT "\-"
DEFINE BAR 10 OF registroro PROMPT "Cancelacion de Comisiones X Pagar"
DEFINE BAR 11 OF registroro PROMPT "\-"
DEFINE BAR 12 OF registroro PROMPT "Recalculo de Rol"
ON SELECTION BAR 1 OF registroro do form rol0302
ON SELECTION BAR 2 OF registroro do form rol0303
ON SELECTION BAR 4 OF registroro do form rol0310
ON SELECTION BAR 6 OF registroro do form rol0315
ON SELECTION BAR 7 OF registroro do form rol0316
ON SELECTION BAR 8 OF registroro do form rol0430a
ON SELECTION BAR 10 OF registroro do form rol0301
ON SELECTION BAR 12 OF registroro do form rol0315e1

DEFINE POPUP reportes MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF reportes PROMPT "Valor a Recibir"
DEFINE BAR 2 OF reportes PROMPT "Liquidacion de sueldo Rubros no Provisiones"
DEFINE BAR 3 OF reportes PROMPT "Resumen mensual"
DEFINE BAR 4 OF reportes PROMPT "Decimo XIII"
DEFINE BAR 5 OF reportes PROMPT "Acumulados"
DEFINE BAR 6 OF reportes PROMPT "Reporte por Rubros"
DEFINE BAR 7 OF reportes PROMPT "Nomina"
DEFINE BAR 8 OF reportes PROMPT "Asistencia y Horas Extras"
DEFINE BAR 9 OF reportes PROMPT "Empleados por Departamento"
DEFINE BAR 10 OF reportes PROMPT "\-"
DEFINE BAR 11 OF reportes PROMPT "Montos Totales (Grupos Doc)"
ON SELECTION BAR 1 OF reportes do form rol0401
ON SELECTION BAR 2 OF reportes do form rol0402
ON SELECTION BAR 3 OF reportes do form rol0403
ON SELECTION BAR 4 OF reportes do form rol0404
ON SELECTION BAR 5 OF reportes do form rol0405
ON SELECTION BAR 6 OF reportes do form rol0406
ON SELECTION BAR 7 OF reportes do form rol0408
ON SELECTION BAR 8 OF reportes do form rol0420
ON SELECTION BAR 9 OF reportes do form rol0402a
ON SELECTION BAR 11 OF reportes do form fac0303

DEFINE POPUP documentos MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF documentos PROMPT "Multiples Valores"
DEFINE BAR 2 OF documentos PROMPT "\-"
DEFINE BAR 3 OF documentos PROMPT "Formas de Pago/Cobro"
DEFINE BAR 4 OF documentos PROMPT "Documento Bancario"
DEFINE BAR 5 OF documentos PROMPT "Correccion"
DEFINE BAR 6 OF documentos PROMPT "Contabilización"
DEFINE BAR 7 OF documentos PROMPT "\-"
DEFINE BAR 8 OF documentos PROMPT "Anulación"
DEFINE BAR 9 OF documentos PROMPT "Consulta"
ON BAR 1 OF documentos ACTIVATE POPUP registrarm
ON SELECTION BAR 3 OF documentos do form adq0701b
ON SELECTION BAR 4 OF documentos do form adq0702b
ON SELECTION BAR 5 OF documentos do form adq0706
ON SELECTION BAR 6 OF documentos do form adq0703b
ON SELECTION BAR 8 OF documentos do form adq0704
ON SELECTION BAR 9 OF documentos do form adq0705b

DEFINE POPUP registrarm MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF registrarm PROMPT "Registrar"
DEFINE BAR 2 OF registrarm PROMPT "Generar Individuales"
DEFINE BAR 3 OF registrarm PROMPT "Cancelar"
DEFINE BAR 4 OF registrarm PROMPT "Contabilizar"
DEFINE BAR 5 OF registrarm PROMPT "\-"
DEFINE BAR 6 OF registrarm PROMPT "Consultar"
ON SELECTION BAR 1 OF registrarm do form docmul
ON SELECTION BAR 2 OF registrarm do form genindi
ON SELECTION BAR 3 OF registrarm do form  cancdocm
ON SELECTION BAR 4 OF registrarm do form contdocm
ON SELECTION BAR 6 OF registrarm do form consdocm

DEFINE POPUP configurac MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF configurac PROMPT "Rubros"
DEFINE BAR 2 OF configurac PROMPT "Contabilizacion de Rubros"
DEFINE BAR 3 OF configurac PROMPT "Turnos"
ON SELECTION BAR 1 OF configurac do form adm0101
ON SELECTION BAR 2 OF configurac do form rol0102
ON SELECTION BAR 3 OF configurac do form rol0103

DEFINE POPUP salir MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF salir PROMPT "Regresar al Menú Princ."
DEFINE BAR 2 OF salir PROMPT "Abandonar el sistema"
ON SELECTION BAR 1 OF salir ;
	DO _2m40z2khn ;
	IN LOCFILE("PROG\MENUROL" ,"MPX;MPR|FXP;PRG" ,"DÓNDE está MENUROL?")
ON SELECTION BAR 2 OF salir ;
	DO _2m40z2kho ;
	IN LOCFILE("PROG\MENUROL" ,"MPX;MPR|FXP;PRG" ,"DÓNDE está MENUROL?")


*       *********************************************************
*       *                                                         
*       *              Limpiar Código y Procedimientos            
*       *                                                         
*       *********************************************************
*

read event

*       *********************************************************
*       *                                                         
*       * _2M40Z2KHN  ON SELECTION BAR 1 OF POPUP salir           
*       *                                                         
*       * Procedure Origin:                                       
*       *                                                         
*       * From Menu:  MENUROL.PRG,            Record:   73        
*       * Called By:  ON SELECTION BAR 1 OF POPUP salir           
*       * Prompt:     Regresar al Menú Princ.                     
*       * Snippet:    1                                           
*       *                                                         
*       *********************************************************
*
PROCEDURE _2m40z2khn
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
*       * _2M40Z2KHO  ON SELECTION BAR 2 OF POPUP salir           
*       *                                                         
*       * Procedure Origin:                                       
*       *                                                         
*       * From Menu:  MENUROL.PRG,            Record:   74        
*       * Called By:  ON SELECTION BAR 2 OF POPUP salir           
*       * Prompt:     Abandonar el sistema                        
*       * Snippet:    2                                           
*       *                                                         
*       *********************************************************
*
PROCEDURE _2m40z2kho
deactivate menu _msysmenu
clea even
clea read
final=1
return


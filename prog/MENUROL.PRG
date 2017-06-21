 SET SYSMENU TO
 SET SYSMENU AUTOMATIC
 DEFINE PAD _2m40z2khh OF _MSYSMENU PROMPT "Ingreso de Datos" KEY ALT+I, "" COLOR SCHEME 3
 DEFINE PAD _2m40z2khi OF _MSYSMENU PROMPT "Registro Rol" KEY ALT+R, "" COLOR SCHEME 3
 DEFINE PAD _2m40z2khj OF _MSYSMENU PROMPT "Reportes" KEY ALT+R, "" COLOR SCHEME 3
 DEFINE PAD _2m40z2khk OF _MSYSMENU PROMPT "Documentos" KEY ALT+D, "" COLOR SCHEME 3
 DEFINE PAD _2m40z2khl OF _MSYSMENU PROMPT "Configuracion" KEY ALT+C, "" COLOR SCHEME 3
 DEFINE PAD _2m40z2khm OF _MSYSMENU PROMPT "Salir" KEY ALT+S, "" COLOR SCHEME 3
 ON PAD _2m40z2khh OF _MSYSMENU ACTIVATE POPUP ingresoded
 ON PAD _2m40z2khi OF _MSYSMENU ACTIVATE POPUP registroro
 ON PAD _2m40z2khj OF _MSYSMENU ACTIVATE POPUP reportes
 ON PAD _2m40z2khk OF _MSYSMENU ACTIVATE POPUP documentos
 ON PAD _2m40z2khl OF _MSYSMENU ACTIVATE POPUP configurac
 ON PAD _2m40z2khm OF _MSYSMENU ACTIVATE POPUP salir
 DEFINE POPUP ingresoded SHADOW MARGIN RELATIVE COLOR SCHEME 4
 DEFINE BAR 1 OF ingresoded PROMPT "Creación de Personas"
 DEFINE BAR 2 OF ingresoded PROMPT "\-"
 DEFINE BAR 3 OF ingresoded PROMPT "Generar Rol"
 DEFINE BAR 4 OF ingresoded PROMPT "\-"
 DEFINE BAR 5 OF ingresoded PROMPT "Rol 2da Forma"
 DEFINE BAR 6 OF ingresoded PROMPT "\-"
 DEFINE BAR 7 OF ingresoded PROMPT "Liquidacion de Sueldo"
 ON BAR 1 OF ingresoded ACTIVATE POPUP creaciónde
 ON SELECTION BAR 3 OF ingresoded DO FORM rol0104
 ON SELECTION BAR 5 OF ingresoded DO FORM rol0108
 ON SELECTION BAR 7 OF ingresoded DO FORM rol0107
 DEFINE POPUP creaciónde SHADOW MARGIN RELATIVE COLOR SCHEME 4
 DEFINE BAR 1 OF creaciónde PROMPT "Datos Generales Sujetos"
 DEFINE BAR 2 OF creaciónde PROMPT "Agregar Datos de Empleados"
 ON SELECTION BAR 1 OF creaciónde DO FORM adm0303
 ON SELECTION BAR 2 OF creaciónde DO FORM rol0101b
 DEFINE POPUP registroro SHADOW MARGIN RELATIVE COLOR SCHEME 4
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
 ON SELECTION BAR 1 OF registroro DO FORM rol0302
 ON SELECTION BAR 2 OF registroro DO FORM rol0303
 ON SELECTION BAR 4 OF registroro DO FORM rol0310
 ON SELECTION BAR 6 OF registroro DO FORM rol0315
 ON SELECTION BAR 7 OF registroro DO FORM rol0316
 ON SELECTION BAR 8 OF registroro DO FORM rol0430a
 ON SELECTION BAR 10 OF registroro DO FORM rol0301
 ON SELECTION BAR 12 OF registroro DO FORM rol0315e1
 DEFINE POPUP reportes SHADOW MARGIN RELATIVE COLOR SCHEME 4
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
 ON SELECTION BAR 1 OF reportes DO FORM rol0401
 ON SELECTION BAR 2 OF reportes DO FORM rol0402
 ON SELECTION BAR 3 OF reportes DO FORM rol0403
 ON SELECTION BAR 4 OF reportes DO FORM rol0404
 ON SELECTION BAR 5 OF reportes DO FORM rol0405
 ON SELECTION BAR 6 OF reportes DO FORM rol0406
 ON SELECTION BAR 7 OF reportes DO FORM rol0408
 ON SELECTION BAR 8 OF reportes DO FORM rol0420
 ON SELECTION BAR 9 OF reportes DO FORM rol0402a
 ON SELECTION BAR 11 OF reportes DO FORM fac0303
 DEFINE POPUP documentos SHADOW MARGIN RELATIVE COLOR SCHEME 4
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
 ON SELECTION BAR 3 OF documentos DO FORM adq0701b
 ON SELECTION BAR 4 OF documentos DO FORM adq0702b
 ON SELECTION BAR 5 OF documentos DO FORM adq0706
 ON SELECTION BAR 6 OF documentos DO FORM adq0703b
 ON SELECTION BAR 8 OF documentos DO FORM adq0704
 ON SELECTION BAR 9 OF documentos DO FORM adq0705b
 DEFINE POPUP registrarm SHADOW MARGIN RELATIVE COLOR SCHEME 4
 DEFINE BAR 1 OF registrarm PROMPT "Registrar"
 DEFINE BAR 2 OF registrarm PROMPT "Generar Individuales"
 DEFINE BAR 3 OF registrarm PROMPT "Cancelar"
 DEFINE BAR 4 OF registrarm PROMPT "Contabilizar"
 DEFINE BAR 5 OF registrarm PROMPT "\-"
 DEFINE BAR 6 OF registrarm PROMPT "Consultar"
 ON SELECTION BAR 1 OF registrarm DO FORM docmul
 ON SELECTION BAR 2 OF registrarm DO FORM genindi
 ON SELECTION BAR 3 OF registrarm DO FORM  cancdocm
 ON SELECTION BAR 4 OF registrarm DO FORM contdocm
 ON SELECTION BAR 6 OF registrarm DO FORM consdocm
 DEFINE POPUP configurac SHADOW MARGIN RELATIVE COLOR SCHEME 4
 DEFINE BAR 1 OF configurac PROMPT "Rubros"
 DEFINE BAR 2 OF configurac PROMPT "Contabilizacion de Rubros"
 DEFINE BAR 3 OF configurac PROMPT "Turnos"
 ON SELECTION BAR 1 OF configurac DO FORM adm0101
 ON SELECTION BAR 2 OF configurac DO FORM rol0102
 ON SELECTION BAR 3 OF configurac DO FORM rol0103
 DEFINE POPUP salir SHADOW MARGIN RELATIVE COLOR SCHEME 4
 DEFINE BAR 1 OF salir PROMPT "Regresar al Menú Princ."
 DEFINE BAR 2 OF salir PROMPT "Abandonar el sistema"
 ON SELECTION BAR 1 OF salir DO _2m40z2khn  IN LOCFILE("PROG\MENUROL" ,"MPX;MPR|FXP;PRG" ,"DÓNDE está MENUROL?")
 ON SELECTION BAR 2 OF salir DO _2m40z2kho  IN LOCFILE("PROG\MENUROL" ,"MPX;MPR|FXP;PRG" ,"DÓNDE está MENUROL?")
 READ EVENTS
ENDPROC
**
PROCEDURE _2m40z2khn
 DO WHILE TYPE('_screen.activeform')='O'
    _SCREEN.activeform.release
 ENDDO
 IF USED('ACCESOS')
    SELECT accesos
    USE
 ENDIF
 DEACTIVATE MENU _MSYSMENU
 CLEAR EVENTS
 CLEAR READ
 RETURN
ENDPROC
**
PROCEDURE _2m40z2kho
 DEACTIVATE MENU _MSYSMENU
 CLEAR EVENTS
 CLEAR READ
 final = 1
 RETURN
ENDPROC
**

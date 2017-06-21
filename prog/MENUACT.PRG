 SET SYSMENU TO
 SET SYSMENU AUTOMATIC
 DEFINE PAD _2k400zlds OF _MSYSMENU PROMPT "Opciones Generales" KEY ALT+O, "" COLOR SCHEME 3
 DEFINE PAD _2k400zldt OF _MSYSMENU PROMPT "Reportes" KEY ALT+R, "" COLOR SCHEME 3
 DEFINE PAD _2k400zldu OF _MSYSMENU PROMPT "Documentos" KEY ALT+D, "" COLOR SCHEME 3
 DEFINE PAD _2k400zldv OF _MSYSMENU PROMPT "Salir" KEY ALT+S, "" COLOR SCHEME 3
 ON PAD _2k400zlds OF _MSYSMENU ACTIVATE POPUP opcionesge
 ON PAD _2k400zldt OF _MSYSMENU ACTIVATE POPUP reportes
 ON PAD _2k400zldu OF _MSYSMENU ACTIVATE POPUP documentos
 ON PAD _2k400zldv OF _MSYSMENU ACTIVATE POPUP salir
 DEFINE POPUP opcionesge SHADOW MARGIN RELATIVE COLOR SCHEME 4
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
 ON SELECTION BAR 1 OF opcionesge DO FORM act0101
 ON SELECTION BAR 2 OF opcionesge DO FORM act0102
 ON SELECTION BAR 3 OF opcionesge DO FORM act0206
 ON SELECTION BAR 4 OF opcionesge DO FORM act0207
 ON BAR 6 OF opcionesge ACTIVATE POPUP depreciaci
 ON SELECTION BAR 11 OF opcionesge DO FORM act0108
 ON SELECTION BAR 12 OF opcionesge DO FORM act0110
 DEFINE POPUP depreciaci SHADOW MARGIN RELATIVE COLOR SCHEME 4
 DEFINE BAR 1 OF depreciaci PROMPT "Calculo"
 DEFINE BAR 2 OF depreciaci PROMPT "Reporte Historico"
 DEFINE BAR 3 OF depreciaci PROMPT "Registro"
 DEFINE BAR 4 OF depreciaci PROMPT "Contabilización"
 ON SELECTION BAR 1 OF depreciaci DO FORM act0104a
 ON SELECTION BAR 2 OF depreciaci DO FORM act0104b
 ON SELECTION BAR 3 OF depreciaci DO FORM act0104c
 ON SELECTION BAR 4 OF depreciaci DO FORM act0212
 DEFINE POPUP reportes SHADOW MARGIN RELATIVE COLOR SCHEME 4
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
 ON SELECTION BAR 1 OF reportes DO FORM act0209
 ON SELECTION BAR 3 OF reportes DO FORM act0210
 ON SELECTION BAR 5 OF reportes DO FORM act0211
 ON SELECTION BAR 6 OF reportes DO FORM act0104b
 ON SELECTION BAR 8 OF reportes DO FORM act0202
 ON SELECTION BAR 9 OF reportes DO FORM act0203
 ON SELECTION BAR 10 OF reportes DO FORM act0204
 ON SELECTION BAR 11 OF reportes DO FORM act0205
 ON SELECTION BAR 12 OF reportes DO FORM act0213
 ON SELECTION BAR 13 OF reportes DO FORM act0208
 DEFINE POPUP documentos SHADOW MARGIN RELATIVE COLOR SCHEME 4
 DEFINE BAR 1 OF documentos PROMPT "Consulta"
 DEFINE BAR 2 OF documentos PROMPT "\-"
 DEFINE BAR 3 OF documentos PROMPT "Formas de Pago/Cobro"
 DEFINE BAR 4 OF documentos PROMPT "\-"
 DEFINE BAR 5 OF documentos PROMPT "Contabilización"
 DEFINE BAR 6 OF documentos PROMPT "\-"
 DEFINE BAR 7 OF documentos PROMPT "Anulación"
 ON SELECTION BAR 1 OF documentos DO FORM adq0705b
 ON SELECTION BAR 3 OF documentos DO FORM adq0701b
 ON SELECTION BAR 5 OF documentos DO FORM adq0703b
 ON SELECTION BAR 7 OF documentos DO FORM adq0704
 DEFINE POPUP salir SHADOW MARGIN RELATIVE COLOR SCHEME 4
 DEFINE BAR 1 OF salir PROMPT "Regresar al Menu Princ."
 DEFINE BAR 2 OF salir PROMPT "Abandonar el sistema"
 ON SELECTION BAR 1 OF salir DO _2k400zldw  IN LOCFILE("PROG\MENUACT" ,"MPX;MPR|FXP;PRG" ,"DÓNDE está MENUACT?")
 ON SELECTION BAR 2 OF salir DO _2k400zldx  IN LOCFILE("PROG\MENUACT" ,"MPX;MPR|FXP;PRG" ,"DÓNDE está MENUACT?")
 READ EVENTS
ENDPROC
**
PROCEDURE _2k400zldw
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
PROCEDURE _2k400zldx
 DEACTIVATE MENU _MSYSMENU
 CLEAR EVENTS
 CLEAR READ
 final = 1
 RETURN
ENDPROC
**

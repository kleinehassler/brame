 q1 = "update detadocb set fecha=rubdoc(code,'fecha')::date "+"where fecha isnull and code in (select code from documents) "
 IF  .NOT. sqli(q1)
    = MESSAGEBOX('Ocurrio un error al confirmar las fechas', 0, empresa)
 ENDIF
 q1 = "update detadocb set isaccount=(rubdoc(code,'isaccount')='t') "+" where code in (select code from documents) "
 IF  .NOT. sqli(q1)
    = MESSAGEBOX('Ocurrio un error al confirmar las contabilizaciones', 0, empresa)
 ENDIF
 WAIT WINDOW NOWAIT 'Proceso de actualizacion exitoso'
 SET SYSMENU TO
 SET SYSMENU AUTOMATIC
 DEFINE PAD _3qp0vd67l OF _MSYSMENU PROMPT "Ingresos" KEY ALT+I, "" COLOR SCHEME 3
 DEFINE PAD _3qp0vd67m OF _MSYSMENU PROMPT "Egresos" KEY ALT+E, "" COLOR SCHEME 3
 DEFINE PAD _3qp0vd67n OF _MSYSMENU PROMPT "Conciliación" KEY ALT+C, "" COLOR SCHEME 3
 DEFINE PAD _3qp0vd67o OF _MSYSMENU PROMPT "Cuentas" KEY ALT+C, "" COLOR SCHEME 3
 DEFINE PAD _3qp0vd67p OF _MSYSMENU PROMPT "Reportes" KEY ALT+R, "" COLOR SCHEME 3
 DEFINE PAD _3qp0vd67q OF _MSYSMENU PROMPT "Documentos" KEY ALT+D, "" COLOR SCHEME 3
 DEFINE PAD _3qp0vd67r OF _MSYSMENU PROMPT "Salir" KEY ALT+S, "" COLOR SCHEME 3
 ON PAD _3qp0vd67l OF _MSYSMENU ACTIVATE POPUP ingresos
 ON PAD _3qp0vd67m OF _MSYSMENU ACTIVATE POPUP egresos
 ON PAD _3qp0vd67n OF _MSYSMENU ACTIVATE POPUP conciliaci
 ON PAD _3qp0vd67o OF _MSYSMENU ACTIVATE POPUP cuentas
 ON PAD _3qp0vd67p OF _MSYSMENU ACTIVATE POPUP reportes
 ON PAD _3qp0vd67q OF _MSYSMENU ACTIVATE POPUP documentos
 ON PAD _3qp0vd67r OF _MSYSMENU ACTIVATE POPUP salir
 DEFINE POPUP ingresos SHADOW MARGIN RELATIVE COLOR SCHEME 4
 DEFINE BAR 1 OF ingresos PROMPT "Cuentas Por Cobrar"
 DEFINE BAR 2 OF ingresos PROMPT "\-"
 DEFINE BAR 3 OF ingresos PROMPT "Otros Ingresos"
 DEFINE BAR 4 OF ingresos PROMPT "Otros Ingresos Confirmados"
 DEFINE BAR 5 OF ingresos PROMPT "Ingresos Det. Sujetos"
 DEFINE BAR 6 OF ingresos PROMPT "Depositos Bancarios (Caja)"
 DEFINE BAR 7 OF ingresos PROMPT "Depositos Bancarios (Cheques Clientes)"
 DEFINE BAR 8 OF ingresos PROMPT "\-"
 DEFINE BAR 9 OF ingresos PROMPT "Liquidaciones"
 ON BAR 1 OF ingresos ACTIVATE POPUP cuentaspor
 ON SELECTION BAR 3 OF ingresos DO FORM ban0102
 ON SELECTION BAR 4 OF ingresos DO FORM ban0102a
 ON SELECTION BAR 5 OF ingresos DO FORM ban0105
 ON SELECTION BAR 6 OF ingresos DO FORM ban0103
 ON SELECTION BAR 7 OF ingresos DO FORM ban0108
 ON SELECTION BAR 9 OF ingresos DO FORM fac0112
 DEFINE POPUP cuentaspor SHADOW MARGIN RELATIVE COLOR SCHEME 4
 DEFINE BAR 1 OF cuentaspor PROMPT "Normal"
 DEFINE BAR 2 OF cuentaspor PROMPT "Creditos Empresariales"
 DEFINE BAR 3 OF cuentaspor PROMPT "Depositos Confirmados"
 ON SELECTION BAR 1 OF cuentaspor DO FORM ban0101
 ON SELECTION BAR 2 OF cuentaspor DO FORM ban0101c
 ON SELECTION BAR 3 OF cuentaspor DO FORM ban0101b
 DEFINE POPUP egresos SHADOW MARGIN RELATIVE COLOR SCHEME 4
 DEFINE BAR 1 OF egresos PROMPT "Impresión de Cheques"
 DEFINE BAR 2 OF egresos PROMPT "Canc. de Cuentas por Pagar"
 DEFINE BAR 3 OF egresos PROMPT "Canc. de Ctas por Pagar Saldo Inic."
 DEFINE BAR 4 OF egresos PROMPT "Retiros"
 DEFINE BAR 5 OF egresos PROMPT "Gastos Bancarios"
 DEFINE BAR 6 OF egresos PROMPT "\-"
 DEFINE BAR 7 OF egresos PROMPT "Pago de Chesques Posfechados"
 DEFINE BAR 8 OF egresos PROMPT "\-"
 DEFINE BAR 9 OF egresos PROMPT "Otros Egresos"
 DEFINE BAR 10 OF egresos PROMPT "\-"
 DEFINE BAR 11 OF egresos PROMPT "Cheques de Otros Periodos"
 ON SELECTION BAR 1 OF egresos DO FORM ban0201
 ON SELECTION BAR 2 OF egresos DO FORM ban0202
 ON SELECTION BAR 3 OF egresos DO FORM ban0203
 ON SELECTION BAR 4 OF egresos DO FORM ban0204
 ON SELECTION BAR 5 OF egresos DO FORM ban0205a
 ON SELECTION BAR 7 OF egresos DO FORM ban0206
 ON SELECTION BAR 9 OF egresos DO FORM ban0207
 ON SELECTION BAR 11 OF egresos DO FORM adq0608
 DEFINE POPUP conciliaci SHADOW MARGIN RELATIVE COLOR SCHEME 4
 DEFINE BAR 1 OF conciliaci PROMPT "Confirmacion de Cheques"
 DEFINE BAR 2 OF conciliaci PROMPT "\-"
 DEFINE BAR 3 OF conciliaci PROMPT "Transferir Cheques a Otro periodo"
 DEFINE BAR 4 OF conciliaci PROMPT "Improtar Cheques otro periodo"
 ON SELECTION BAR 1 OF conciliaci DO FORM ban0301
 ON SELECTION BAR 3 OF conciliaci DO FORM ban0304
 ON SELECTION BAR 4 OF conciliaci DO FORM ban0304a
 DEFINE POPUP cuentas SHADOW MARGIN RELATIVE COLOR SCHEME 4
 DEFINE BAR 1 OF cuentas PROMPT "Cuentas bancarias"
 DEFINE BAR 2 OF cuentas PROMPT "Saldos Iniciales"
 ON SELECTION BAR 1 OF cuentas DO FORM ban0401
 ON SELECTION BAR 2 OF cuentas DO FORM ban0402
 DEFINE POPUP reportes SHADOW MARGIN RELATIVE COLOR SCHEME 4
 DEFINE BAR 1 OF reportes PROMPT "Conciliacion"
 DEFINE BAR 2 OF reportes PROMPT "Cheques Pendientes"
 DEFINE BAR 3 OF reportes PROMPT "Estado de Cuenta"
 DEFINE BAR 4 OF reportes PROMPT "Estado de Cuenta General"
 DEFINE BAR 5 OF reportes PROMPT "Documentos Bancarios"
 DEFINE BAR 6 OF reportes PROMPT "Cheques posfechados"
 DEFINE BAR 7 OF reportes PROMPT "Documentos Anulados"
 DEFINE BAR 8 OF reportes PROMPT "Montos Totales (Grupos Doc)"
 ON BAR 1 OF reportes ACTIVATE POPUP _22b0yjdyg
 ON SELECTION BAR 2 OF reportes DO FORM ban0502
 ON SELECTION BAR 3 OF reportes DO FORM ban0503
 ON SELECTION BAR 4 OF reportes DO FORM ban0504
 ON SELECTION BAR 5 OF reportes DO FORM ban0505
 ON SELECTION BAR 6 OF reportes DO FORM ban0507
 ON SELECTION BAR 7 OF reportes DO FORM ban0508
 ON SELECTION BAR 8 OF reportes DO FORM fac0303a
 DEFINE POPUP _22b0yjdyg SHADOW MARGIN RELATIVE COLOR SCHEME 4
 DEFINE BAR 1 OF _22b0yjdyg PROMPT "Con Fechas doc bancario"
 DEFINE BAR 2 OF _22b0yjdyg PROMPT "Con Fechas de Emisión"
 ON SELECTION BAR 1 OF _22b0yjdyg DO FORM ban0501
 ON SELECTION BAR 2 OF _22b0yjdyg DO FORM ban0501a
 DEFINE POPUP documentos SHADOW MARGIN RELATIVE COLOR SCHEME 4
 DEFINE BAR 1 OF documentos PROMPT "Autorización"
 DEFINE BAR 2 OF documentos PROMPT "Formas de Pago/Cobro"
 DEFINE BAR 3 OF documentos PROMPT "Generar Doc. Bancario"
 DEFINE BAR 4 OF documentos PROMPT "Contabilización"
 DEFINE BAR 5 OF documentos PROMPT "\-"
 DEFINE BAR 6 OF documentos PROMPT "Consulta Doc. Bancarios"
 DEFINE BAR 7 OF documentos PROMPT "\-"
 DEFINE BAR 8 OF documentos PROMPT "Consulta Documentos"
 DEFINE BAR 9 OF documentos PROMPT "Anulacion"
 DEFINE BAR 10 OF documentos PROMPT "\-"
 DEFINE BAR 11 OF documentos PROMPT "Anulacion Doc. bancario"
 DEFINE BAR 12 OF documentos PROMPT "Cambio de Cheque"
 ON SELECTION BAR 1 OF documentos DO FORM adq0707
 ON SELECTION BAR 2 OF documentos DO FORM adq0701b
 ON SELECTION BAR 3 OF documentos DO FORM adq0702b
 ON SELECTION BAR 4 OF documentos DO FORM adq0703b
 ON SELECTION BAR 6 OF documentos DO FORM ban0604
 ON SELECTION BAR 8 OF documentos DO FORM adq0705b
 ON SELECTION BAR 9 OF documentos DO FORM adq0704
 ON SELECTION BAR 11 OF documentos DO FORM ban0606
 ON SELECTION BAR 12 OF documentos DO FORM ban0607
 DEFINE POPUP salir SHADOW MARGIN RELATIVE COLOR SCHEME 4
 DEFINE BAR 1 OF salir PROMPT "Regresar al Menú Princ."
 DEFINE BAR 2 OF salir PROMPT "Abandonar el sistema"
 ON SELECTION BAR 1 OF salir DO _3qp0vd67s  IN LOCFILE("PROG\MENUBAN" ,"MPX;MPR|FXP;PRG" ,"DÓNDE está MENUBAN?")
 ON SELECTION BAR 2 OF salir DO _3qp0vd67t  IN LOCFILE("PROG\MENUBAN" ,"MPX;MPR|FXP;PRG" ,"DÓNDE está MENUBAN?")
 READ EVENTS
ENDPROC
**
PROCEDURE _3qp0vd67s
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
PROCEDURE _3qp0vd67t
 FOR i = 1 TO 15
    RELEASE WINDOW
 ENDFOR
 DEACTIVATE MENU _MSYSMENU
 CLEAR EVENTS
 CLEAR READ
 final = 1
 RETURN
ENDPROC
**

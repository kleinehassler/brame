 SET SYSMENU TO
 SET SYSMENU AUTOMATIC
 DEFINE PAD _2sw00kkyh OF _MSYSMENU PROMPT "Plan de Cuentas" KEY ALT+P, "" COLOR SCHEME 3
 DEFINE PAD _2sw00kkyi OF _MSYSMENU PROMPT "Saldos Iniciales" KEY ALT+S, "" COLOR SCHEME 3
 DEFINE PAD _2sw00kkyj OF _MSYSMENU PROMPT "Reportes" KEY ALT+R, "" COLOR SCHEME 3
 DEFINE PAD _2sw00kkyk OF _MSYSMENU PROMPT "Ajustes" KEY ALT+A, "" COLOR SCHEME 3
 DEFINE PAD _2sw00kkyl OF _MSYSMENU PROMPT "Documentos" KEY ALT+D, "" COLOR SCHEME 3
 DEFINE PAD _2sw00kkym OF _MSYSMENU PROMPT "Salir" KEY ALT+S, "" COLOR SCHEME 3
 ON PAD _2sw00kkyh OF _MSYSMENU ACTIVATE POPUP plandecuen
 ON PAD _2sw00kkyi OF _MSYSMENU ACTIVATE POPUP _2gc0q3cdj
 ON PAD _2sw00kkyj OF _MSYSMENU ACTIVATE POPUP reportes
 ON PAD _2sw00kkyk OF _MSYSMENU ACTIVATE POPUP ajustes
 ON PAD _2sw00kkyl OF _MSYSMENU ACTIVATE POPUP _1s110jpeh
 ON PAD _2sw00kkym OF _MSYSMENU ACTIVATE POPUP salir
 DEFINE POPUP plandecuen SHADOW MARGIN RELATIVE COLOR SCHEME 4
 DEFINE BAR 1 OF plandecuen PROMPT "Edición del Plan de Cuentas"
 ON SELECTION BAR 1 OF plandecuen DO FORM con0201
 DEFINE POPUP _2gc0q3cdj SHADOW MARGIN RELATIVE COLOR SCHEME 4
 DEFINE BAR 1 OF _2gc0q3cdj PROMPT "Importar Saldos  Periodo Anterior"
 DEFINE BAR 2 OF _2gc0q3cdj PROMPT "Importar detalle de Saldos Periodo anterior"
 DEFINE BAR 3 OF _2gc0q3cdj PROMPT "\-"
 DEFINE BAR 4 OF _2gc0q3cdj PROMPT "Edicion"
 DEFINE BAR 5 OF _2gc0q3cdj PROMPT "Edicion detalle"
 DEFINE BAR 6 OF _2gc0q3cdj PROMPT "Borrar saldos"
 DEFINE BAR 7 OF _2gc0q3cdj PROMPT "Contabilizar Cheques de Clientes"
 DEFINE BAR 8 OF _2gc0q3cdj PROMPT "\-"
 DEFINE BAR 9 OF _2gc0q3cdj PROMPT "Generar Archivo de Saldos Prox Periodo"
 DEFINE BAR 10 OF _2gc0q3cdj PROMPT "Generar Detalle de Saldos Iniciales Prox Periodo"
 ON SELECTION BAR 1 OF _2gc0q3cdj DO FORM con0204a
 ON SELECTION BAR 2 OF _2gc0q3cdj DO FORM con0207a
 ON BAR 4 OF _2gc0q3cdj ACTIVATE POPUP edicion
 ON SELECTION BAR 5 OF _2gc0q3cdj DO FORM con0206
 ON SELECTION BAR 6 OF _2gc0q3cdj DO FORM con0205
 ON SELECTION BAR 7 OF _2gc0q3cdj DO FORM fac0210
 ON SELECTION BAR 9 OF _2gc0q3cdj DO FORM con0204
 ON SELECTION BAR 10 OF _2gc0q3cdj DO FORM con0207
 DEFINE POPUP edicion SHADOW MARGIN RELATIVE COLOR SCHEME 4
 DEFINE BAR 1 OF edicion PROMPT "Por Lista"
 DEFINE BAR 2 OF edicion PROMPT "Por codigo"
 ON SELECTION BAR 1 OF edicion DO FORM con0203
 ON SELECTION BAR 2 OF edicion DO FORM con0203a
 DEFINE POPUP reportes SHADOW MARGIN RELATIVE COLOR SCHEME 4
 DEFINE BAR 1 OF reportes PROMPT "Cuentas Contables"
 DEFINE BAR 2 OF reportes PROMPT "Balances"
 DEFINE BAR 3 OF reportes PROMPT "Documentos"
 DEFINE BAR 4 OF reportes PROMPT "\-"
 DEFINE BAR 5 OF reportes PROMPT "Detalle Saldos Iniciales"
 DEFINE BAR 6 OF reportes PROMPT "Estado de Cuenta Sujetos"
 DEFINE BAR 7 OF reportes PROMPT "Costo De Ventas"
 DEFINE BAR 8 OF reportes PROMPT "\-"
 DEFINE BAR 9 OF reportes PROMPT "Verificacion de Contabilizacion"
 ON BAR 1 OF reportes ACTIVATE POPUP cuentascon
 ON BAR 2 OF reportes ACTIVATE POPUP balances
 ON BAR 3 OF reportes ACTIVATE POPUP nuevoeleme
 ON BAR 5 OF reportes ACTIVATE POPUP detallesal
 ON BAR 6 OF reportes ACTIVATE POPUP estadodecu
 ON SELECTION BAR 7 OF reportes DO FORM con0113
 ON SELECTION BAR 9 OF reportes DO FORM verificacont
 DEFINE POPUP cuentascon SHADOW MARGIN RELATIVE COLOR SCHEME 4
 DEFINE BAR 1 OF cuentascon PROMPT "Plan de cuentas"
 DEFINE BAR 2 OF cuentascon PROMPT "Detalle de Auxiliar de Cuenta"
 DEFINE BAR 3 OF cuentascon PROMPT "Cuentas Pendientes del Ejercicio"
 DEFINE BAR 4 OF cuentascon PROMPT "Historico detallado de una Cuenta Resumen"
 DEFINE BAR 5 OF cuentascon PROMPT "Historico Detallado de una Cuenta"
 DEFINE BAR 6 OF cuentascon PROMPT "Saldos Deudores y Acreedores"
 DEFINE BAR 7 OF cuentascon PROMPT "Resumen Ciertas Cuentas"
 DEFINE BAR 8 OF cuentascon PROMPT "Resumen Ciertas Ctas Detalle"
 ON SELECTION BAR 1 OF cuentascon DO FORM con0101
 ON SELECTION BAR 2 OF cuentascon DO FORM con0103
 ON SELECTION BAR 3 OF cuentascon DO FORM con0104
 ON SELECTION BAR 4 OF cuentascon DO FORM con0110
 ON SELECTION BAR 5 OF cuentascon DO FORM fac0311b
 ON SELECTION BAR 6 OF cuentascon DO FORM con0105
 ON SELECTION BAR 7 OF cuentascon DO FORM con0114
 ON SELECTION BAR 8 OF cuentascon DO FORM con0114a
 DEFINE POPUP balances SHADOW MARGIN RELATIVE COLOR SCHEME 4
 DEFINE BAR 1 OF balances PROMPT "Comprobación"
 DEFINE BAR 2 OF balances PROMPT "Comprobacion por sujeto"
 DEFINE BAR 3 OF balances PROMPT "Sumas y Saldos"
 DEFINE BAR 4 OF balances PROMPT "General"
 DEFINE BAR 5 OF balances PROMPT "General por Periodos"
 DEFINE BAR 6 OF balances PROMPT "Estado de Perdidas y Ganancias"
 DEFINE BAR 7 OF balances PROMPT "Estado de Perdidas y Ganancias por Periodo"
 ON SELECTION BAR 1 OF balances DO FORM con0106
 ON SELECTION BAR 2 OF balances DO FORM con0106a
 ON SELECTION BAR 3 OF balances DO FORM con0107
 ON SELECTION BAR 4 OF balances DO FORM con0108
 ON SELECTION BAR 5 OF balances DO FORM con0108a
 ON SELECTION BAR 6 OF balances DO FORM con0109
 ON SELECTION BAR 7 OF balances DO FORM con0109a
 DEFINE POPUP nuevoeleme SHADOW MARGIN RELATIVE COLOR SCHEME 4
 DEFINE BAR 1 OF nuevoeleme PROMPT "En Orden Secuencia"
 DEFINE BAR 2 OF nuevoeleme PROMPT "Doc Bines-Servicios"
 DEFINE BAR 3 OF nuevoeleme PROMPT "Monto Totales (grupo doc)"
 DEFINE BAR 4 OF nuevoeleme PROMPT "Documentos no Contabiilizados"
 DEFINE BAR 5 OF nuevoeleme PROMPT "Secuencial de Asientos"
 DEFINE BAR 6 OF nuevoeleme PROMPT "Resumen de Asientos Contables"
 DEFINE BAR 7 OF nuevoeleme PROMPT "Doc. Descuadrados"
 ON SELECTION BAR 1 OF nuevoeleme DO FORM con0102
 ON SELECTION BAR 2 OF nuevoeleme DO FORM con0112
 ON SELECTION BAR 3 OF nuevoeleme DO FORM fac0303a
 ON SELECTION BAR 4 OF nuevoeleme DO FORM con0115
 ON SELECTION BAR 5 OF nuevoeleme DO FORM con0116
 ON SELECTION BAR 6 OF nuevoeleme DO FORM con0116a
 ON SELECTION BAR 7 OF nuevoeleme DO FORM con0117
 DEFINE POPUP detallesal SHADOW MARGIN RELATIVE COLOR SCHEME 4
 DEFINE BAR 1 OF detallesal PROMPT "Por cuenta"
 DEFINE BAR 2 OF detallesal PROMPT "Detalle de Pagos"
 DEFINE BAR 3 OF detallesal PROMPT "Descuadrados"
 DEFINE BAR 4 OF detallesal PROMPT "No registrados"
 ON SELECTION BAR 1 OF detallesal DO FORM con0208
 ON SELECTION BAR 2 OF detallesal DO FORM con0209
 ON SELECTION BAR 3 OF detallesal DO FORM con0210
 ON SELECTION BAR 4 OF detallesal DO FORM con0211
 DEFINE POPUP estadodecu SHADOW MARGIN RELATIVE COLOR SCHEME 4
 DEFINE BAR 1 OF estadodecu PROMPT "Normal"
 DEFINE BAR 2 OF estadodecu PROMPT "Det Saldos Inic."
 ON SELECTION BAR 1 OF estadodecu DO FORM con0111
 ON SELECTION BAR 2 OF estadodecu DO FORM con0111a
 DEFINE POPUP ajustes SHADOW MARGIN RELATIVE COLOR SCHEME 4
 DEFINE BAR 1 OF ajustes PROMPT "Transferencia de Valores Caja"
 DEFINE BAR 2 OF ajustes PROMPT "Prestamos Socios"
 DEFINE BAR 3 OF ajustes PROMPT "Transf. de Valores Ctas. Por Pagar"
 DEFINE BAR 4 OF ajustes PROMPT "Varios"
 DEFINE BAR 5 OF ajustes PROMPT "Varios Det. Sujetos"
 DEFINE BAR 6 OF ajustes PROMPT "Varios Det. Items"
 DEFINE BAR 7 OF ajustes PROMPT "Ingresos/Egresos"
 DEFINE BAR 8 OF ajustes PROMPT "Liquidaciones"
 ON SELECTION BAR 1 OF ajustes DO FORM con0302
 ON SELECTION BAR 2 OF ajustes DO FORM con0303
 ON SELECTION BAR 3 OF ajustes DO FORM con0304
 ON SELECTION BAR 4 OF ajustes DO FORM con0305
 ON BAR 5 OF ajustes ACTIVATE POPUP variosdets
 ON BAR 6 OF ajustes ACTIVATE POPUP variosdeti
 ON SELECTION BAR 7 OF ajustes DO FORM con0308
 ON SELECTION BAR 8 OF ajustes DO FORM con0309
 DEFINE POPUP variosdets SHADOW MARGIN RELATIVE COLOR SCHEME 4
 DEFINE BAR 1 OF variosdets PROMPT "Por Cuenta Contable"
 DEFINE BAR 2 OF variosdets PROMPT "Por Valor"
 DEFINE BAR 3 OF variosdets PROMPT "Cred. Empresas"
 ON SELECTION BAR 1 OF variosdets DO FORM con0306
 ON SELECTION BAR 2 OF variosdets DO FORM con0306b
 ON SELECTION BAR 3 OF variosdets DO FORM con0306c
 DEFINE POPUP variosdeti SHADOW MARGIN RELATIVE COLOR SCHEME 4
 DEFINE BAR 1 OF variosdeti PROMPT "Por Cuentas"
 DEFINE BAR 2 OF variosdeti PROMPT "Por Valor"
 ON SELECTION BAR 1 OF variosdeti DO FORM con0307a
 ON SELECTION BAR 2 OF variosdeti DO FORM con0307b
 DEFINE POPUP _1s110jpeh SHADOW MARGIN RELATIVE COLOR SCHEME 4
 DEFINE BAR 1 OF _1s110jpeh PROMPT "Forma de Pago"
 DEFINE BAR 2 OF _1s110jpeh PROMPT "Doc. Bancario"
 DEFINE BAR 3 OF _1s110jpeh PROMPT "Contabilizacion"
 DEFINE BAR 4 OF _1s110jpeh PROMPT "\-"
 DEFINE BAR 5 OF _1s110jpeh PROMPT "Correccion"
 DEFINE BAR 6 OF _1s110jpeh PROMPT "Anulacion"
 DEFINE BAR 7 OF _1s110jpeh PROMPT "\-"
 DEFINE BAR 8 OF _1s110jpeh PROMPT "Consulta"
 ON SELECTION BAR 1 OF _1s110jpeh DO FORM adq0701b
 ON SELECTION BAR 2 OF _1s110jpeh DO FORM adq0702b
 ON BAR 3 OF _1s110jpeh ACTIVATE POPUP contabiliz
 ON SELECTION BAR 5 OF _1s110jpeh DO FORM adq0706
 ON SELECTION BAR 6 OF _1s110jpeh DO FORM adq0704
 ON SELECTION BAR 8 OF _1s110jpeh DO FORM adq0705b
 DEFINE POPUP contabiliz SHADOW MARGIN RELATIVE COLOR SCHEME 4
 DEFINE BAR 1 OF contabiliz PROMPT "Individual Detallada"
 DEFINE BAR 2 OF contabiliz PROMPT "Por Grupos"
 DEFINE BAR 3 OF contabiliz PROMPT "Individual"
 DEFINE BAR 4 OF contabiliz PROMPT "Ajustes Contables"
 ON SELECTION BAR 1 OF contabiliz DO FORM adq0703b
 ON SELECTION BAR 2 OF contabiliz DO FORM contadoc
 ON SELECTION BAR 3 OF contabiliz DO FORM adq0703c
 ON SELECTION BAR 4 OF contabiliz DO FORM adq0703d
 DEFINE POPUP salir SHADOW MARGIN RELATIVE COLOR SCHEME 4
 DEFINE BAR 1 OF salir PROMPT "Regresal al Menú Princ."
 DEFINE BAR 2 OF salir PROMPT "Abandonar el sistema"
 ON SELECTION BAR 1 OF salir DO _2sw00kkyw  IN LOCFILE("PROG\MENUCON" ,"MPX;MPR|FXP;PRG" ,"DÓNDE está MENUCON?")
 ON SELECTION BAR 2 OF salir DO _2sw00kkyx  IN LOCFILE("PROG\MENUCON" ,"MPX;MPR|FXP;PRG" ,"DÓNDE está MENUCON?")
 READ EVENTS
ENDPROC
**
PROCEDURE _2sw00kkyw
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
PROCEDURE _2sw00kkyx
 DEACTIVATE MENU _MSYSMENU
 CLEAR EVENTS
 CLEAR READ
 final = 1
 RETURN
ENDPROC
**

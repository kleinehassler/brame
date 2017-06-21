 SET SYSMENU TO
 SET SYSMENU AUTOMATIC
 DEFINE PAD _3r50reays OF _MSYSMENU PROMPT "Ventas" KEY ALT+V, "" COLOR SCHEME 3
 DEFINE PAD _3r50reayt OF _MSYSMENU PROMPT "Clientes" KEY ALT+C, "" COLOR SCHEME 3
 DEFINE PAD _3r50reayu OF _MSYSMENU PROMPT "Reportes" KEY ALT+R, "" COLOR SCHEME 3
 DEFINE PAD _3r50reayv OF _MSYSMENU PROMPT "N/C y N/D" KEY ALT+N, "" COLOR SCHEME 3
 DEFINE PAD _3r50reayw OF _MSYSMENU PROMPT "Impresión Documentos" KEY ALT+I, "" COLOR SCHEME 3
 DEFINE PAD _3r50reayx OF _MSYSMENU PROMPT "Documentos" KEY ALT+D, "" COLOR SCHEME 3
 DEFINE PAD _3r50reayy OF _MSYSMENU PROMPT "Salir" KEY ALT+S, "" COLOR SCHEME 3
 ON PAD _3r50reays OF _MSYSMENU ACTIVATE POPUP ventas
 ON PAD _3r50reayt OF _MSYSMENU ACTIVATE POPUP nuevoeleme
 ON PAD _3r50reayu OF _MSYSMENU ACTIVATE POPUP reportes
 ON PAD _3r50reayv OF _MSYSMENU ACTIVATE POPUP ncynd
 ON PAD _3r50reayw OF _MSYSMENU ACTIVATE POPUP impresiónd
 ON PAD _3r50reayx OF _MSYSMENU ACTIVATE POPUP documentos
 ON PAD _3r50reayy OF _MSYSMENU ACTIVATE POPUP salir
 DEFINE POPUP ventas SHADOW MARGIN RELATIVE COLOR SCHEME 4
 DEFINE BAR 1 OF ventas PROMPT "Doc. Ventas"
 DEFINE BAR 2 OF ventas PROMPT "Doc. Ventas Personal"
 DEFINE BAR 3 OF ventas PROMPT "Cotizaciones"
 DEFINE BAR 4 OF ventas PROMPT "\-"
 DEFINE BAR 5 OF ventas PROMPT "Canje de Retenciones"
 DEFINE BAR 6 OF ventas PROMPT "Anticipos Clientes"
 DEFINE BAR 7 OF ventas PROMPT "\-"
 DEFINE BAR 8 OF ventas PROMPT "liquidacions en Ventas"
 DEFINE BAR 9 OF ventas PROMPT "\-"
 DEFINE BAR 10 OF ventas PROMPT "Servicios"
 DEFINE BAR 11 OF ventas PROMPT "Sujetos"
 ON SELECTION BAR 1 OF ventas DO FORM fac0101a
 ON SELECTION BAR 2 OF ventas DO FORM fac0102a
 ON SELECTION BAR 3 OF ventas DO FORM fac0103a
 ON SELECTION BAR 5 OF ventas DO FORM  fac0107
 ON SELECTION BAR 6 OF ventas DO FORM fac0104
 ON SELECTION BAR 8 OF ventas DO FORM fac0113
 ON SELECTION BAR 10 OF ventas DO FORM adq0109
 ON SELECTION BAR 11 OF ventas DO FORM adm0303
 DEFINE POPUP nuevoeleme SHADOW MARGIN RELATIVE COLOR SCHEME 4
 DEFINE BAR 1 OF nuevoeleme PROMPT "Cheques Recibidos"
 DEFINE BAR 2 OF nuevoeleme PROMPT "Confirmacion de Cheques"
 DEFINE BAR 3 OF nuevoeleme PROMPT "Cheques otro Periodo"
 DEFINE BAR 4 OF nuevoeleme PROMPT "\-"
 DEFINE BAR 5 OF nuevoeleme PROMPT "Actualizar datos de Cheques"
 DEFINE BAR 6 OF nuevoeleme PROMPT "\-"
 DEFINE BAR 7 OF nuevoeleme PROMPT "Otros Creditos"
 ON SELECTION BAR 1 OF nuevoeleme DO FORM cheques
 ON BAR 2 OF nuevoeleme ACTIVATE POPUP confirmaci
 ON SELECTION BAR 3 OF nuevoeleme DO FORM chequesind
 ON SELECTION BAR 5 OF nuevoeleme DO FORM actuche
 ON SELECTION BAR 7 OF nuevoeleme DO FORM cresuj
 DEFINE POPUP confirmaci SHADOW MARGIN RELATIVE COLOR SCHEME 4
 DEFINE BAR 1 OF confirmaci PROMPT "Individual"
 DEFINE BAR 2 OF confirmaci PROMPT "Por fechas"
 ON SELECTION BAR 1 OF confirmaci DO FORM confirche
 ON SELECTION BAR 2 OF confirmaci DO FORM confirche2
 DEFINE POPUP reportes SHADOW MARGIN RELATIVE COLOR SCHEME 4
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
 ON SELECTION BAR 11 OF reportes DO FORM fac0308
 DEFINE POPUP porcliente SHADOW MARGIN RELATIVE COLOR SCHEME 4
 DEFINE BAR 1 OF porcliente PROMPT "Principales"
 DEFINE BAR 2 OF porcliente PROMPT "Ventas"
 DEFINE BAR 3 OF porcliente PROMPT "Historico"
 DEFINE BAR 4 OF porcliente PROMPT "Estado de Cuenta"
 DEFINE BAR 5 OF porcliente PROMPT "Cheques"
 DEFINE BAR 6 OF porcliente PROMPT "Cheques NO Registrados"
 DEFINE BAR 7 OF porcliente PROMPT "Cheques por Vecimiento"
 DEFINE BAR 8 OF porcliente PROMPT "Consulta Cheques"
 DEFINE BAR 9 OF porcliente PROMPT "Ultimas Transacciones"
 DEFINE BAR 10 OF porcliente PROMPT "Por Documento"
 DEFINE BAR 11 OF porcliente PROMPT "Saldos con Intereses"
 DEFINE BAR 12 OF porcliente PROMPT "Saldos Iniciales"
 DEFINE BAR 13 OF porcliente PROMPT "Por Montos"
 ON SELECTION BAR 1 OF porcliente DO FORM impprin
 ON SELECTION BAR 2 OF porcliente DO FORM fac0301
 ON SELECTION BAR 3 OF porcliente DO FORM fac0304
 ON BAR 4 OF porcliente ACTIVATE POPUP estadodecu
 ON SELECTION BAR 5 OF porcliente DO FORM fac0312
 ON SELECTION BAR 6 OF porcliente DO FORM fac0312a
 ON SELECTION BAR 7 OF porcliente DO FORM checliven
 ON SELECTION BAR 8 OF porcliente DO FORM consulche
 ON SELECTION BAR 9 OF porcliente DO FORM fac0320
 ON SELECTION BAR 10 OF porcliente DO FORM fac0305
 ON SELECTION BAR 11 OF porcliente DO FORM fac0322
 ON SELECTION BAR 12 OF porcliente DO FORM fac0324
 ON SELECTION BAR 13 OF porcliente DO FORM fac0326
 DEFINE POPUP estadodecu SHADOW MARGIN RELATIVE COLOR SCHEME 4
 DEFINE BAR 1 OF estadodecu PROMPT "Normal"
 DEFINE BAR 2 OF estadodecu PROMPT "Det Saldos Iniciales"
 ON SELECTION BAR 1 OF estadodecu DO FORM con0111
 ON SELECTION BAR 2 OF estadodecu DO FORM con0111a
 DEFINE POPUP cajacredit SHADOW MARGIN RELATIVE COLOR SCHEME 4
 DEFINE BAR 1 OF cajacredit PROMPT "Ingresos - Egresos (Contable)"
 DEFINE BAR 2 OF cajacredit PROMPT "Ingresos-Egresos (det canc)"
 DEFINE BAR 3 OF cajacredit PROMPT "Movimiento de la Cta Caja"
 DEFINE BAR 4 OF cajacredit PROMPT "Detalle de Pagos Grupo"
 DEFINE BAR 5 OF cajacredit PROMPT "Detalle  de Recaudacion Resumen diario"
 DEFINE BAR 6 OF cajacredit PROMPT "Detalle de Pagos"
 DEFINE BAR 7 OF cajacredit PROMPT "\-"
 DEFINE BAR 8 OF cajacredit PROMPT "Pagos Descuadrados"
 ON SELECTION BAR 1 OF cajacredit DO FORM con0116a
 ON SELECTION BAR 2 OF cajacredit DO FORM fac0311
 ON SELECTION BAR 3 OF cajacredit DO FORM fac0311a
 ON SELECTION BAR 4 OF cajacredit DO FORM  fac0319
 ON SELECTION BAR 5 OF cajacredit DO FORM fac0311c
 ON SELECTION BAR 6 OF cajacredit DO FORM  fac0319a
 ON SELECTION BAR 8 OF cajacredit DO FORM fac0323
 DEFINE POPUP poritems SHADOW MARGIN RELATIVE COLOR SCHEME 4
 DEFINE BAR 1 OF poritems PROMPT "Documentos Por Items"
 DEFINE BAR 2 OF poritems PROMPT "Ventas por Items"
 DEFINE BAR 3 OF poritems PROMPT "Rentabilidad"
 DEFINE BAR 4 OF poritems PROMPT "Articulos No vendidos"
 ON SELECTION BAR 1 OF poritems DO FORM fac0302
 ON SELECTION BAR 2 OF poritems DO FORM fac0315
 ON SELECTION BAR 3 OF poritems DO FORM fac0317
 ON SELECTION BAR 4 OF poritems DO FORM fac0327
 DEFINE POPUP portransac SHADOW MARGIN RELATIVE COLOR SCHEME 4
 DEFINE BAR 1 OF portransac PROMPT "Montos Totales"
 DEFINE BAR 2 OF portransac PROMPT "Montos Totales (Grupos Doc)"
 DEFINE BAR 3 OF portransac PROMPT "Montos Totales Recaudacion"
 DEFINE BAR 4 OF portransac PROMPT "Montos Totales Recaudacion por Cta"
 DEFINE BAR 5 OF portransac PROMPT "Montos Totales Recaudacion Varios Clientes"
 DEFINE BAR 6 OF portransac PROMPT "Resumen Por Mes"
 DEFINE BAR 7 OF portransac PROMPT "Retenciones Recibidas"
 DEFINE BAR 8 OF portransac PROMPT "Terceros"
 DEFINE BAR 9 OF portransac PROMPT "Por Vencimiento"
 ON SELECTION BAR 1 OF portransac DO FORM fac0303
 ON SELECTION BAR 2 OF portransac DO FORM fac0303a
 ON SELECTION BAR 3 OF portransac DO FORM fac0303c
 ON SELECTION BAR 4 OF portransac DO FORM fac0303e
 ON SELECTION BAR 5 OF portransac DO FORM fac0303f
 ON SELECTION BAR 6 OF portransac DO FORM fac0316
 ON SELECTION BAR 7 OF portransac DO FORM retenfac
 ON SELECTION BAR 8 OF portransac DO FORM rol0407
 ON BAR 9 OF portransac ACTIVATE POPUP porfechade
 DEFINE POPUP porfechade SHADOW MARGIN RELATIVE COLOR SCHEME 4
 DEFINE BAR 1 OF porfechade PROMPT "Por Fechas"
 DEFINE BAR 2 OF porfechade PROMPT "Varias Cuentas"
 DEFINE BAR 3 OF porfechade PROMPT "Resumen"
 DEFINE BAR 4 OF porfechade PROMPT "Vencidos y Por Vencer"
 ON SELECTION BAR 1 OF porfechade DO FORM fac0310
 ON SELECTION BAR 2 OF porfechade DO FORM fac0310a
 ON SELECTION BAR 3 OF porfechade DO FORM fac0310b
 ON SELECTION BAR 4 OF porfechade DO FORM fac0310c
 DEFINE POPUP ventasporv SHADOW MARGIN RELATIVE COLOR SCHEME 4
 DEFINE BAR 1 OF ventasporv PROMPT "Por Ventas"
 DEFINE BAR 2 OF ventasporv PROMPT "Por Recaudacion"
 DEFINE BAR 3 OF ventasporv PROMPT "\-"
 DEFINE BAR 4 OF ventasporv PROMPT "Guias"
 ON SELECTION BAR 1 OF ventasporv DO FORM fac0309
 ON SELECTION BAR 2 OF ventasporv DO FORM fac0309a
 ON SELECTION BAR 4 OF ventasporv DO FORM inv0602b
 DEFINE POPUP ncynd SHADOW MARGIN RELATIVE COLOR SCHEME 4
 DEFINE BAR 1 OF ncynd PROMPT "Nota de Credito o Debito Clientes (Devoluciones)"
 DEFINE BAR 2 OF ncynd PROMPT "Nota de Credito o Debito Clientes (Descuentos)"
 DEFINE BAR 3 OF ncynd PROMPT "\-"
 DEFINE BAR 4 OF ncynd PROMPT "Nota de Credito o Debito Clientes (Devoluciones Sin Ref.)"
 DEFINE BAR 5 OF ncynd PROMPT "Nota de Credito o Debito Clientes (Descuentos Sin Ref.)"
 ON SELECTION BAR 1 OF ncynd DO FORM fac0401
 ON SELECTION BAR 2 OF ncynd DO FORM fac0402
 ON SELECTION BAR 4 OF ncynd DO FORM fac0401a
 ON SELECTION BAR 5 OF ncynd DO FORM fac0402a
 DEFINE POPUP impresiónd SHADOW MARGIN RELATIVE COLOR SCHEME 4
 DEFINE BAR 1 OF impresiónd PROMPT "Activar Impresión de Documentos"
 DEFINE BAR 2 OF impresiónd PROMPT "Desactivar Impresión de Doc. Ventas"
 ON SELECTION BAR 1 OF impresiónd DO FORM actimpdoc
 ON SELECTION BAR 2 OF impresiónd DO _3r50reayz  IN LOCFILE("PROG\MENUFAC" ,"MPX;MPR|FXP;PRG" ,"DÓNDE está MENUFAC?")
 DEFINE POPUP documentos SHADOW MARGIN RELATIVE COLOR SCHEME 4
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
 ON SELECTION BAR 1 OF documentos DO FORM adq0701b
 ON SELECTION BAR 2 OF documentos DO FORM saldoini
 ON SELECTION BAR 3 OF documentos DO FORM adq0702b
 ON SELECTION BAR 4 OF documentos DO FORM adq0703b
 ON SELECTION BAR 6 OF documentos DO FORM adq0707
 ON SELECTION BAR 7 OF documentos DO FORM adq0704
 ON SELECTION BAR 9 OF documentos DO FORM adq0705b
 ON SELECTION BAR 11 OF documentos DO FORM adq0706
 ON SELECTION BAR 12 OF documentos DO FORM costoven
 DEFINE POPUP salir SHADOW MARGIN RELATIVE COLOR SCHEME 4
 DEFINE BAR 1 OF salir PROMPT "Regresar al Menú Princ."
 DEFINE BAR 2 OF salir PROMPT "Abandonar el Sistema"
 ON SELECTION BAR 1 OF salir DO _3r50reaz5  IN LOCFILE("PROG\MENUFAC" ,"MPX;MPR|FXP;PRG" ,"DÓNDE está MENUFAC?")
 ON SELECTION BAR 2 OF salir DO _3r50reaz7  IN LOCFILE("PROG\MENUFAC" ,"MPX;MPR|FXP;PRG" ,"DÓNDE está MENUFAC?")
 READ EVENTS
ENDPROC
**
PROCEDURE _3r50reayz
 IF TYPE('_screen.objimp')='O'
    _SCREEN.removeobject('objimp')
 ENDIF
ENDPROC
**
PROCEDURE _3r50reaz5
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
PROCEDURE _3r50reaz7
 DEACTIVATE MENU _MSYSMENU
 CLEAR EVENTS
 CLEAR READ
 final = 1
 RETURN
ENDPROC
**

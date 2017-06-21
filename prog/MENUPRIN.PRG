*** 
*** ReFox XI+  #GT059442  Administrador  EMBRACE [VFP60]
***
 CLEAR ALL
 CLEAR
 LOCAL bodega
 bodega = ''
 DO inicio.fxp
 DO principal81.fxp
 WITH _SCREEN
    .caption = ''
    .icon = '..\varios\FACE02.ICO'
    .fontname = 'arial'
    .fontsize = 10
    .fontbold = .F.
    .borderstyle = 2
    .maxbutton = .F.
    .width = 800
    .height = 520
    .backcolor = RGB(192, 192, 192)
    .closable = .F.
    .addobject('logo', 'image')
    .logo.width = 720
    .logo.height = 560
    .logo.stretch = 1
    .logo.backstyle = 0
    .refresh
 ENDWITH
 CLEAR
 IF SCOLS()<133 .OR. SROWS()<33
    WITH _SCREEN
       .windowstate = 2
       .maxbutton = .T.
       .refresh
    ENDWITH
    CLEAR
 ENDIF
 SET SYSMENU TO
 WITH _SCREEN
    .logo.visible = .F.
    .logo.left = (.width-.logo.width)/2
    .logo.top = (.height-.logo.height-100)/2+100
    .addobject('cuadro1', 'shape')
    .cuadro1.width = .width-20
    .cuadro1.height = 90
    .cuadro1.borderwidth = 3
    .cuadro1.backstyle = 0
    .cuadro1.visible = .T.
    .cuadro1.top = 5
    .cuadro1.left = (.width-.cuadro1.width)/2
    .addobject('cuadro2', 'shape')
    .cuadro2.width = .width-20
    .cuadro2.height = .height-105
    .cuadro2.borderwidth = 3
    .cuadro2.backstyle = 0
    .cuadro2.visible = .T.
    .cuadro2.top = 98
    .cuadro2.left = (.width-.cuadro2.width)/2
 ENDWITH
 DO FORM clave TO final
 _SCREEN.caption = ALLTRIM(empresa)
 q1 = "select descripda as cbodega "+"from detagru "+"where iddato="+alup(idb)+";"
 IF sqli(q1, 'bodega')
    SELECT bodega
    bodega = bodega.cbodega
    SELECT bodega
    USE
 ELSE
    WAIT WINDOW NOWAIT 'Error en obtención de bodega'
 ENDIF
 IF final=0
    IF SQLEXEC(nconec, "select now()::date as fecha;", 'fechahoy')>0
       SELECT fechahoy
       hoy = fecha
       SELECT fechahoy
       USE
    ELSE
       WAIT WINDOW NOWAIT 'No existe comunicación'
    ENDIF
    DO defvariables
    IF  .NOT. configura()
       final = 1
    ENDIF
 ENDIF
 WITH _SCREEN
    .caption = ALLTRIM(empresa)
    .addobject('titulo', 'Label')
    .titulo.backstyle = 0
    .titulo.fontshadow = .T.
    .titulo.fontunderline = .T.
    .titulo.caption = ALLTRIM(empresa)
    .titulo.fontname = 'Times New Roman'
    .titulo.fontsize = 16
    .titulo.fontbold = .T.
    .titulo.autosize = .T.
    .titulo.left = 50
    .titulo.top = 15
    .titulo.visible = .T.
    .titulo.left = (.width-.titulo.width)/2
    .addobject('periodo', 'Label')
    .periodo.fontsize = 10
    .periodo.backstyle = 0
    .periodo.forecolor = RGB(0, 128, 64)
    .periodo.autosize = .T.
    .periodo.fontbold = .T.
    .periodo.caption = 'USUARIO: '+ALLTRIM(usuario)+SPACE(5)+'PERIODO: '+ALLTRIM(devfeclet(finip))+'  al  '+ALLTRIM(devfeclet(ffinp))
    .periodo.fontname = 'Times New Roman'
    .periodo.left = 50
    .periodo.top = 75
    .periodo.visible = .T.
    .periodo.left = (.width-.periodo.width)/2
    ccaja = ''
    q1 = "select sname "+"from sujetos "+"where idsujeto="+alup(idcajero)
    IF sqli(q1, 'nomcaj')
       ccaja = nomcaj.sname
    ENDIF
    .addobject('fecequi', 'Label')
    .fecequi.fontsize = 10
    .fecequi.backstyle = 0
    .fecequi.autosize = .T.
    .fecequi.fontbold = .T.
    .fecequi.caption = 'EQUIPO: '+equipo+SPACE(5)+'CAJA: '+ALLTRIM(ccaja)+SPACE(5)+'FECHA: '+ALLTRIM(devfeclet(hoy))+SPACE(5)+'AGENCIA: '+ALLTRIM(agencia)
    .fecequi.fontname = 'Times New Roman'
    .fecequi.left = 50
    .fecequi.top = 105
    .fecequi.visible = .T.
    .fecequi.left = (.width-.fecequi.width)/2
    .addobject('modulo', 'Label')
    .modulo.fontsize = 14
    .modulo.backstyle = 0
    .modulo.forecolor = RGB(0, 0, 128)
    .modulo.autosize = .T.
    .modulo.fontbold = .T.
    .modulo.caption = ''
    .modulo.fontname = 'Times New Roman'
    .modulo.left = 50
    .modulo.top = 50
    .modulo.visible = .T.
 ENDWITH
 IF escaja .AND. final=0
    DO menucaj
 ELSE
    DEFINE POPUP sisconin FROM 8, 5 RELATIVE SHORTCUT
    DEFINE BAR 1 OF sisconin PROMPT "ADMINISTRACION" FONT 'Times New Roman', 12 STYLE 'B' MESSAGE 'Mantenimiento de las opciones Generales' SKIP FOR .F.
    DEFINE BAR 3 OF sisconin PROMPT "ADQUISICIONES" FONT 'Times New Roman', 12 STYLE 'B' MESSAGE '' SKIP FOR .F.
    DEFINE BAR 4 OF sisconin PROMPT "FACTURACION" FONT 'Times New Roman', 12 STYLE 'B' MESSAGE '' SKIP FOR .F.
    DEFINE BAR 5 OF sisconin PROMPT "CONTABILIDAD" FONT 'Times New Roman', 12 STYLE 'B' MESSAGE '' SKIP FOR .F.
    DEFINE BAR 6 OF sisconin PROMPT "INVENTARIO" FONT 'Times New Roman', 12 STYLE 'B' MESSAGE '' SKIP FOR .F.
    DEFINE BAR 7 OF sisconin PROMPT "BANCOS" FONT 'Times New Roman', 12 STYLE 'B' MESSAGE '' SKIP FOR .F.
    DEFINE BAR 8 OF sisconin PROMPT "ROL DE PAGOS" FONT 'Times New Roman', 12 STYLE 'B' MESSAGE '' SKIP FOR .F.
    DEFINE BAR 11 OF sisconin PROMPT "ACTIVOS FIJOS" FONT 'Times New Roman', 12 STYLE 'B' MESSAGE '' SKIP FOR .F.
    DEFINE BAR 12 OF sisconin PROMPT "S. R. I." FONT 'Times New Roman', 12 STYLE 'B' MESSAGE '' SKIP FOR .F.
    DEFINE BAR 14 OF sisconin PROMPT "CONFIGURACION" FONT 'Times New Roman', 12 STYLE 'B' MESSAGE '' SKIP FOR .F.
    DEFINE BAR 15 OF sisconin PROMPT "\-"
    DEFINE BAR 16 OF sisconin PROMPT "SALIR" FONT 'Times New Roman', 12 STYLE 'B' MESSAGE 'Abandonar el Sistema' SKIP FOR .F.
    ON SELECTION BAR 1 OF sisconin DO programa WITH 'menuadm'
    ON SELECTION BAR 3 OF sisconin DO programa WITH 'menuadq'
    ON SELECTION BAR 4 OF sisconin DO programa WITH 'menufac'
    ON SELECTION BAR 5 OF sisconin DO programa WITH 'menucon'
    ON SELECTION BAR 6 OF sisconin DO programa WITH 'menuinv'
    ON SELECTION BAR 7 OF sisconin DO programa WITH 'menuban'
    ON SELECTION BAR 8 OF sisconin DO programa WITH 'menurol'
    ON SELECTION BAR 11 OF sisconin DO programa WITH 'menuact'
    ON SELECTION BAR 12 OF sisconin DO programa WITH 'menusri'
    ON SELECTION BAR 14 OF sisconin DO programa WITH 'menuset'
    ON SELECTION BAR 16 OF sisconin FINAL=1
    DO WHILE  .NOT. (final=1)
       ACTIVATE POPUP sisconin
    ENDDO
    q1 = "delete from autprecio where idsujeto="+alup(idu)
    = sqli(q1)
 ENDIF
 = SQLDISCONNECT(nconec)
 CLOSE ALL
 f = SYS(2003)+'\'+nombdd
 DELETE DATA &f
 QUIT
ENDPROC
**
PROCEDURE programa
 PARAMETER p
 DO CASE
    CASE p='menucaj'
       menux = 'C A J A'
       modulok = 'CAJ'
    CASE p='menuadm'
       menux = 'Administración Maestro'
       modulok = 'ADM'
    CASE p='menusri'
       menux = 'Rentas'
       modulok = 'SRI'
    CASE p='menuadq'
       menux = 'Adquisiciones'
       modulok = 'ADQ'
    CASE p='menufac'
       menux = 'Facturación'
       modulok = 'FAC'
    CASE p='menucon'
       menux = 'Contabilidad'
       modulok = 'CON'
    CASE p='menurol'
       menux = 'Rol de Pagos'
       modulok = 'ROL'
    CASE p='menupro'
       menux = 'Producción'
       modulok = 'PRO'
    CASE p='menuset'
       menux = 'Configuración'
       modulok = 'SET'
    CASE p='menuinv'
       menux = 'Inventario'
       modulok = 'INV'
    CASE p='menuban'
       menux = 'Bancos'
       modulok = 'BAN'
    CASE p='menuact'
       menux = 'Activos Fijos'
       modulok = 'ACT'
    CASE p='menurec'
       menux = 'Recursos Humanos'
       modulok = 'REC'
    OTHERWISE
       RETURN
 ENDCASE
 _SCREEN.modulo.caption = menux
 _SCREEN.modulo.left = (_SCREEN.width-_SCREEN.modulo.width)/2
 _SCREEN.caption = ALLTRIM(empresa)
 DO &p
 _SCREEN.modulo.caption = ' '
 _SCREEN.modulo.left = (_SCREEN.width-_SCREEN.modulo.width)/2
ENDPROC
**
*** 
*** ReFox - todo no se pierde 
***

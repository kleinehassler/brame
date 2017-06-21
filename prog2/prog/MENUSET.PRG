*       *********************************************************
*       *                                                         
*       * 26/12/2011            MENUSET.PRG             11:29:16  
*       *                                                         
*       *********************************************************
*       *                                                         
*       * Nombre del autor                                        
*       *                                                         
*       * Copyright (C) 2011 Nombre de compañía                   
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

DEFINE PAD _3e50omem8 OF _MSYSMENU PROMPT "Accesos" COLOR SCHEME 3 ;
	KEY ALT+A, ""
DEFINE PAD _3e50omem9 OF _MSYSMENU PROMPT "Empresas" COLOR SCHEME 3 ;
	KEY ALT+E, ""
DEFINE PAD _3e50omema OF _MSYSMENU PROMPT "Datos Generales" COLOR SCHEME 3 ;
	KEY ALT+D, ""
DEFINE PAD _3e50omemb OF _MSYSMENU PROMPT "Mi configuración" COLOR SCHEME 3 ;
	KEY ALT+M, ""
DEFINE PAD _3e50omemc OF _MSYSMENU PROMPT "Reportes" COLOR SCHEME 3 ;
	KEY ALT+R, ""
DEFINE PAD _3e50omemd OF _MSYSMENU PROMPT "Agencias" COLOR SCHEME 3 ;
	KEY ALT+A, ""
DEFINE PAD _3e50omeme OF _MSYSMENU PROMPT "Salir" COLOR SCHEME 3 ;
	KEY ALT+S, ""
ON PAD _3e50omem8 OF _MSYSMENU ACTIVATE POPUP accesos
ON PAD _3e50omem9 OF _MSYSMENU ACTIVATE POPUP empresas
ON PAD _3e50omema OF _MSYSMENU ACTIVATE POPUP datosgener
ON PAD _3e50omemb OF _MSYSMENU ACTIVATE POPUP miconfigur
ON PAD _3e50omemc OF _MSYSMENU ACTIVATE POPUP reportes
ON PAD _3e50omemd OF _MSYSMENU ACTIVATE POPUP nuevoeleme
ON PAD _3e50omeme OF _MSYSMENU ACTIVATE POPUP salir

DEFINE POPUP accesos MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF accesos PROMPT "Programas"
DEFINE BAR 2 OF accesos PROMPT "Usuarios"
DEFINE BAR 3 OF accesos PROMPT "Estaciones de Trabajo"
DEFINE BAR 4 OF accesos PROMPT "\-"
DEFINE BAR 5 OF accesos PROMPT "Restriccion x Usuario"
DEFINE BAR 6 OF accesos PROMPT "Restriccion x Perfil"
DEFINE BAR 7 OF accesos PROMPT "Anulaciones y Autorizaciones por Usuario"
DEFINE BAR 8 OF accesos PROMPT "\-"
DEFINE BAR 9 OF accesos PROMPT "Autorizacion de Precios"
ON SELECTION BAR 1 OF accesos do form set0101
ON SELECTION BAR 2 OF accesos do form set0102
ON SELECTION BAR 3 OF accesos do form set0103
ON SELECTION BAR 5 OF accesos do form set0104
ON SELECTION BAR 6 OF accesos do form set0105
ON SELECTION BAR 7 OF accesos do form set0106
ON SELECTION BAR 9 OF accesos do form set0108

DEFINE POPUP empresas MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF empresas PROMPT "Creación - Edición"
DEFINE BAR 2 OF empresas PROMPT "Periodos"
DEFINE BAR 3 OF empresas PROMPT "Crear nuevo periodo"
DEFINE BAR 4 OF empresas PROMPT "Iniciar Periodo"
DEFINE BAR 5 OF empresas PROMPT "Transferir Auxiliares"
ON SELECTION BAR 1 OF empresas do form set0201
ON SELECTION BAR 2 OF empresas do form set0202
ON SELECTION BAR 3 OF empresas do form set0203
ON SELECTION BAR 4 OF empresas do form set0204
ON BAR 5 OF empresas ACTIVATE POPUP transferir

DEFINE POPUP transferir MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF transferir PROMPT "Exportar"
DEFINE BAR 2 OF transferir PROMPT "Importar"
ON SELECTION BAR 1 OF transferir do form set0205a
ON SELECTION BAR 2 OF transferir do form set0205b

DEFINE POPUP datosgener MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF datosgener PROMPT "Grupos de Datos"
DEFINE BAR 2 OF datosgener PROMPT "Datos Generales"
DEFINE BAR 3 OF datosgener PROMPT "\-"
DEFINE BAR 4 OF datosgener PROMPT "Secuencias"
DEFINE BAR 5 OF datosgener PROMPT "Variables del sistema"
ON SELECTION BAR 1 OF datosgener do form set0301
ON SELECTION BAR 2 OF datosgener do form set0302
ON SELECTION BAR 4 OF datosgener do form set0303
ON SELECTION BAR 5 OF datosgener do form set0305

DEFINE POPUP miconfigur MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF miconfigur PROMPT "Configuración"
DEFINE BAR 2 OF miconfigur PROMPT "Cambia Clave"
DEFINE BAR 3 OF miconfigur PROMPT "Cambiar Clave otro Usuario"
DEFINE BAR 4 OF miconfigur PROMPT "Impresion Tarjeta"
DEFINE BAR 5 OF miconfigur PROMPT "Otra Empresa y/o Periodo"
DEFINE BAR 6 OF miconfigur PROMPT "Apariencia de Formularios"
DEFINE BAR 7 OF miconfigur PROMPT "Actualizar Acceso a Programas"
DEFINE BAR 8 OF miconfigur PROMPT "\-"
DEFINE BAR 9 OF miconfigur PROMPT "Respaldar Base de Datos"
DEFINE BAR 10 OF miconfigur PROMPT "Optimizar Rendimiento"
ON SELECTION BAR 1 OF miconfigur do form configura
ON SELECTION BAR 2 OF miconfigur do form set0401
ON SELECTION BAR 3 OF miconfigur Do form set0401a
ON SELECTION BAR 4 OF miconfigur do form set0402
ON SELECTION BAR 5 OF miconfigur ;
	DO _3e50omemf ;
	IN LOCFILE("PROG\MENUSET" ,"MPX;MPR|FXP;PRG" ,"DÓNDE está MENUSET?")
ON SELECTION BAR 6 OF miconfigur do form set0403
ON SELECTION BAR 7 OF miconfigur ;
	DO _3e50omemg ;
	IN LOCFILE("PROG\MENUSET" ,"MPX;MPR|FXP;PRG" ,"DÓNDE está MENUSET?")
ON SELECTION BAR 9 OF miconfigur do form respaldo
ON SELECTION BAR 10 OF miconfigur do form vacuum

DEFINE POPUP reportes MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF reportes PROMPT "Transacciones Anuladas"
DEFINE BAR 2 OF reportes PROMPT "Transacciones Erradas"
DEFINE BAR 3 OF reportes PROMPT "Total de Transacciones"
DEFINE BAR 4 OF reportes PROMPT "\-"
DEFINE BAR 5 OF reportes PROMPT "Bitacora de Registros"
DEFINE BAR 6 OF reportes PROMPT "\-"
DEFINE BAR 7 OF reportes PROMPT "Transacciones Erradas"
ON SELECTION BAR 1 OF reportes do form adm0601
ON SELECTION BAR 2 OF reportes do form adm0602
ON SELECTION BAR 3 OF reportes do form adm0603
ON SELECTION BAR 5 OF reportes do form adm0604
ON SELECTION BAR 7 OF reportes do form adm0605

DEFINE POPUP nuevoeleme MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF nuevoeleme PROMPT "Tablas"
DEFINE BAR 2 OF nuevoeleme PROMPT "Secuencias de Tablas"
DEFINE BAR 3 OF nuevoeleme PROMPT "\-"
DEFINE BAR 4 OF nuevoeleme PROMPT "Actualizar Secuencias"
DEFINE BAR 5 OF nuevoeleme PROMPT "\-"
DEFINE BAR 6 OF nuevoeleme PROMPT "Envio de Datos"
DEFINE BAR 7 OF nuevoeleme PROMPT "Recepción de Datos"
ON SELECTION BAR 1 OF nuevoeleme do form set0502
ON SELECTION BAR 2 OF nuevoeleme do form set0501
ON SELECTION BAR 4 OF nuevoeleme do form set0505
ON SELECTION BAR 6 OF nuevoeleme do form set0503
ON SELECTION BAR 7 OF nuevoeleme do form set0504

DEFINE POPUP salir MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF salir PROMPT "Regresar al Menú Princ."
DEFINE BAR 2 OF salir PROMPT "Abandonar el Sistema"
ON SELECTION BAR 1 OF salir ;
	DO _3e50omemn ;
	IN LOCFILE("PROG\MENUSET" ,"MPX;MPR|FXP;PRG" ,"DÓNDE está MENUSET?")
ON SELECTION BAR 2 OF salir ;
	DO _3e50omemo ;
	IN LOCFILE("PROG\MENUSET" ,"MPX;MPR|FXP;PRG" ,"DÓNDE está MENUSET?")


*       *********************************************************
*       *                                                         
*       *              Limpiar Código y Procedimientos            
*       *                                                         
*       *********************************************************
*

read event

*       *********************************************************
*       *                                                         
*       * _3E50OMEMF  ON SELECTION BAR 5 OF POPUP miconfigur      
*       *                                                         
*       * Procedure Origin:                                       
*       *                                                         
*       * From Menu:  MENUSET.PRG,            Record:   39        
*       * Called By:  ON SELECTION BAR 5 OF POPUP miconfigur      
*       * Prompt:     Otra Empresa y/o Periodo                    
*       * Snippet:    1                                           
*       *                                                         
*       *********************************************************
*
PROCEDURE _3e50omemf
do form clave
with _screen
	.titulo.caption=alltrim(empresa)
	.titulo.left=(.width - .titulo.width)/2

	.periodo.caption='USUARIO: '+alltrim(USUARIO)+space(10)+'PERIODO: '+alltrim(devfeclet(finip))+;
	'  al  '+alltrim(devfeclet(ffinp))+space(4)
	.periodo.left=(.width - .periodo.width)/2

	.fecequi.caption='EQUIPO: '+equipo+SPACE(5)+'FECHA: '+alltrim(DEVFECLET(HOY))
	.fecequi.left=(.width - .fecequi.width)/2
	.refresh
endwith


*       *********************************************************
*       *                                                         
*       * _3E50OMEMG  ON SELECTION BAR 7 OF POPUP miconfigur      
*       *                                                         
*       * Procedure Origin:                                       
*       *                                                         
*       * From Menu:  MENUSET.PRG,            Record:   41        
*       * Called By:  ON SELECTION BAR 7 OF POPUP miconfigur      
*       * Prompt:     Actualizar Acceso a Programas               
*       * Snippet:    2                                           
*       *                                                         
*       *********************************************************
*
PROCEDURE _3e50omemg
if !used('accesos') then
	sele accesos
	use
endif

q1="select distinct p.progname, p.cgdoc, p.cdocu, a.actu, a.inser, a.anu "+;
	"from accesos a, programas p "+;
	"where p.idprograma=a.idprog and a.idsujeto="+alup(idu)+;
		" and p.estapro union "+;
			"(select distinct p.progname, p.cgdoc, p.cdocu, a.actu, a.inser, a.anu "+;
			"from acceper a, programas p, usuarios u "+;
			"where p.idprograma=a.idprog and a.idperfil=u.tipou "+;
			" and p.estapro and u.idsujeto="+alup(idu)+") order by progname;"
			
if !sqli(q1,'accesos') then
	wait 'No se actualizo!!!' wind nowait
else
	wait 'Actualización realizada' wind nowait
endif
	



*       *********************************************************
*       *                                                         
*       * _3E50OMEMN  ON SELECTION BAR 1 OF POPUP salir           
*       *                                                         
*       * Procedure Origin:                                       
*       *                                                         
*       * From Menu:  MENUSET.PRG,            Record:   69        
*       * Called By:  ON SELECTION BAR 1 OF POPUP salir           
*       * Prompt:     Regresar al Menú Princ.                     
*       * Snippet:    3                                           
*       *                                                         
*       *********************************************************
*
PROCEDURE _3e50omemn
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
*       * _3E50OMEMO  ON SELECTION BAR 2 OF POPUP salir           
*       *                                                         
*       * Procedure Origin:                                       
*       *                                                         
*       * From Menu:  MENUSET.PRG,            Record:   70        
*       * Called By:  ON SELECTION BAR 2 OF POPUP salir           
*       * Prompt:     Abandonar el Sistema                        
*       * Snippet:    4                                           
*       *                                                         
*       *********************************************************
*
PROCEDURE _3e50omemo
deactivate menu _msysmenu
clea even
clea read
final=1
return


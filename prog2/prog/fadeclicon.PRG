local rango, r

rango=50

CIUDAD=3892
TIPCLI=5716

q1="ALTER SEQUENCE detagru_iddato_seq RESTART WITH 12500"
=sqli(q1)


q1="delete from detagru where idgrupo=23 and iddato<>"+alup(3892)
=sqli(q1)

select distinct ciudad from clientes where !empty(alltrim(ciudad)) into cursor ciudades

sele ciudades
GO TOP
do while !eof()
	q1="insert into detagru ( descripda, idgrupo) values "+;
			pal(ciudad)+ual(23)
				
	=sqli(q1)
		
	skip
enddo

q1="delete from detagru where idgrupo=238 "
=sqli(q1)

select distinct tipnegocio from clientes where !empty(alltrim(tipnegocio)) into cursor actividad

sele actividad
GO TOP
do while !eof()
	q1="insert into detagru ( descripda, idgrupo) values "+;
			pal(tipnegocio)+ual(238)
				
	=sqli(q1)
		
	skip
enddo

if used('zonasm')
	sele zonasm
else
	sele 0
	use zonasm
endif

q1="delete from detagru where idgrupo=203 and iddato<>"+alup(12574)
=sqli(q1)

sele zonasm
GO TOP
do while !eof()
	q1="insert into detagru ( descripda, valorda, idgrupo) values "+;
			pal(descripcio)+al(codigo)+ual(203)
				
	=sqli(q1)
		
	skip
enddo

if !dg('CIUD')
	return
endif

if !dg('ACTI')
	return
endif

if !dg('ZONA')
	return
endif

x=0
q1="begin"
if !sqli(q1)
	return
endif

q1="delete from sujetos where not isempleado "
if !sqli(q1)
	return
endif

do fintrans

q1="ALTER SEQUENCE auxiliarescon_seq RESTART WITH 200"
=sqli(q1)

IF !USED('clientes')
	USE clientes.dbf IN 0 SHARED
ENDIF

SELE clientes

replace all nombre with strtran(nombre,'-','')
replace all nombre with strtran(nombre,"'",'')

replace all ssri with strtran(ssri,'-','')
replace all ssri with strtran(ssri,"'",'')

replace all direc with strtran(direc,'-','')
replace all direc with strtran(direc,"'",'')

replace all fono1 with strtran(fono1,'-','')
replace all fono1 with strtran(fono1,' ','')

replace all fono2 with strtran(fono2,'-','')
replace all fono2 with strtran(fono2,' ','')

replace all fax with strtran(fax,'-','')
replace all fax with strtran(fax,' ','')

GO TOP
SCAN
	q1="select max(scode)::int4 as scode from sujetos ;"

	if sqli(q1,'numcod') then
		ncodigo=scode+1
	endif


	cate=5716

	prov=19

	idsuj=0
	q1="select nextsecu('auxiliarescon_seq'::text, 'SUJETOS'::text) as idsujeto"
	if sqli(q1,'suj')
		idsuj=suj.idsujeto
	endif

	ciud=3892
	sele ciudades
	go top
	locate for alltrim(descripda)=alltrim(clientes.ciudad)
	if found()
		ciud=ciudades.iddato
	endif

	sele zonas
	locate for alltrim(descripda)='NINGUNA'
	if found()
		zon=iddato
	else
		wait 'No existe zona' wind
		return
	endif
	
	go top
	locate for alltrim(valorda)=alltrim(clientes.zona)
	if found()
		zon=zonas.iddato
	endif
	
	sele actividad
	locate for alltrim(descripda)='CONSUMIDOR FINAL'
	if found()
		act=iddato
	else
		wait 'No existe actividad' wind
		return
	endif

	go top
	locate for alltrim(descripda)=alltrim(clientes.tipnegocio)
	if found()
		act=actividad.iddato
	endif

	sele clientes
	q1="insert into sujetos ( idsujeto, scode, isempleado, isproveedor, isproductor, iscliente, issocio, sname, ssri, "+;
				" saddr, stelf, scel, sfax, smoney, sprov, sciudad, numord, tipcli, scedula, sruc, scredito, scheques, "+;
				" snumcasa, tipact, zona, dimension, fec_aper, smail ) values "+;
				pal(idsuj)+al(ncodigo) +al(.f.) +al(.f.) +al(.f.) +al(.t.) +al(.f.) +;
				al(alltrim(nombre)) + al(iif(empty(ssri),alltrim(nombre),ssri)) + al(direc) +al(fono1)+ ;
				al(fono2)+al(fax)+al(2) +al(prov) +al(ciud) +al(1) +al(cate)+;
				al(iif(len(alltrim(ruc))=10,alltrim(ruc),''))+al(iif(len(alltrim(ruc))=13,alltrim(ruc),''))+;
				al(iif(credito,1,0))+al(iif(cheque,1,0))+al(int(val(numcasa)))+al(act)+ al(zon) + ;
				al(iif(substr(tamano,1,1)='3',3,iif(substr(tamano,1,1)='2',2,1)))+ ;
				al(iif(empty(fec_ing),date(2009,01,01),  date(val(substr(fec_ing,7,4)),val(substr(fec_ing,4,2)),val(substr(fec_ing,1,2))) )) +;
				ual(email)
	if sqli(q1)
		wait 'Registrado...'+alltrim(clientes.nombre) wind at 2,2 nowait
	endif
		
	sele clientes
ENDSCAN

q1="ALTER SEQUENCE auxiliarescon_seq RESTART WITH 8000"
=sqli(q1)

wait 'Proceso concluido...' wind nowait

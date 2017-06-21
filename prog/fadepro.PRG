local rango, r

rango=50

CIUDAD=3892
TIPCLI=5716

select distinct ciudad from proveedores where !empty(alltrim(ciudad)) into cursor ciudades

sele ciudades
GO TOP
do while !eof()
	q1="select iddato from detagru where descripda="+alup(ciudad)+" and idgrupo=23"
	if sqli(q1,'cpro')
		if reccount('cpro')=0
			q1="insert into detagru ( descripda, idgrupo) values "+;
					pal(ciudad)+ual(23)
						
			=sqli(q1)
		endif
	endif
	sele ciudades	
	skip
enddo


if !dg('CIUD')
	return
endif

x=0
q1="begin"
if !sqli(q1)
	return
endif

q1="ALTER SEQUENCE auxiliarescon_seq RESTART WITH 6500"
=sqli(q1)

q1="delete from sujetos where not isempleado and isproveedor "
if !sqli(q1)
	return
endif

do fintrans

IF !USED('proveedores')
	USE proveedores.dbf IN 0 SHARED
ENDIF

SELE proveedores

replace all nombre with strtran(nombre,'-','')
replace all nombre with strtran(nombre,"'",'')

replace all contacto with strtran(contacto,'-','')
replace all contacto with strtran(contacto,"'",'')

replace all saddr with strtran(saddr,'-','')
replace all saddr with strtran(saddr,"'",'')

replace all telefono with strtran(telefono,'-','')
replace all telefono with strtran(telefono,' ','')

replace all telefono2 with strtran(telefono2,'-','')
replace all telefono2 with strtran(telefono2,' ','')


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
	locate for alltrim(descripda)=alltrim(proveedores.ciudad)
	if found()
		ciud=ciudades.iddato
	endif


	sele proveedores
	q1="insert into sujetos ( idsujeto, scode, isempleado, isproveedor, isproductor, iscliente, issocio, sname, ssri, "+;
				" saddr, stelf, scel, smoney, sprov, sciudad, numord, scedula, sruc, "+;
				" smail ) values "+;
				pal(idsuj)+al(ncodigo) +al(.f.) +al(.t.) +al(.f.) +al(.f.) +al(.f.) +;
				al(alltrim(nombre)) + al(alltrim(nombre)) + al(saddr) +al(telefono)+ ;
				al(telefono2)+al(2) +al(prov) +al(ciud) +al(1) +;
				al(iif(len(alltrim(ruc))=10,alltrim(ruc),''))+al(iif(len(alltrim(ruc))=13,alltrim(ruc),''))+;
				ual(email)
	if sqli(q1)
		wait 'Registrado...'+alltrim(proveedores.nombre) wind at 2,2 nowait
	endif
		
	sele proveedores
ENDSCAN


wait 'Proceso concluido...' wind nowait

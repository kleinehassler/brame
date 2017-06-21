CIUDAD=3892

TIPCLI=5716

IF !USED('clientesvar')
	USE clientesvar.dbf IN 0 SHARED
ENDIF

SELE clientesvar
replace all nombre with strtran(nombre,'-','')
replace all nombre with strtran(nombre,"'",'')

replace all direccion1 with strtran(direccion1,'-','')
replace all direccion1 with strtran(direccion1,"'",'')

replace all telefono with strtran(telefono,'-','')
replace all telefono with strtran(telefono,' ','')

GO TOP
SCAN
	ncodigo=0
	q1="select max(scode)::int4 as coda from sujetos "
	if sqli(q1,'coda')
		sele coda
		ncodigo=iif(isnull(coda.coda),1,coda.coda+1)
	endif
	
	cate=5716

	ciud=3892
	
	prov=19
	
	sele clientesvar

	r=iif(len(alltrim(ruc))=10,"scedula="+alup(ruc),;
	  iif(len(alltrim(ruc))=13,"sruc="+alup(ruc),;
	  	  alup(.t.)))

	q1="select scode from sujetos where "+r
	if sqli(q1,'sujeto')
		if reccount('sujeto')=0
			sele clientesvar
			q1="insert into sujetos ( scode, isempleado, isproveedor, isproductor, iscliente, issocio, sname, ssri, "+;
						" saddr, stelf, smoney, sprov, sciudad, numord, tipcli, scedula, sruc) values "+;
						pal(ncodigo) +al(.f.) +al(.f.) +al(.f.) +al(.t.) +al(.f.) +;
						al(alltrim(nombre)) + al(nombre) + al(direccion1) +al(telefono)+ ;
						al(2) +al(prov) +al(ciud) +al(1) +al(cate)+;
						al(iif(len(alltrim(ruc))=10,alltrim(ruc),''))+ual(iif(len(alltrim(ruc))=13,alltrim(ruc),''))
			if sqli(q1)
				wait 'Registrado...'+alltrim(nombre) wind at 2,2 nowait
			endif
		endif
	else
		wait 'error' wind nowait
	endif
		
	sele clientesvar
ENDSCAN

wait 'Proceso concluido...' wind nowait

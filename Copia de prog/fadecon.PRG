local rango

rango=19500

CIUDAD=3892

TIPCLI=5716

select distinct grupo, contacto, 0000000 as idsujeto ;
from clientes ;
where !empty(alltrim(grupo)) and !empty(alltrim(contacto)) ;
group by grupo into table \frigocentro\migrar\contacto

SELE contacto
replace all contacto with strtran(contacto,'-','')
replace all contacto with strtran(contacto,"'",'')

replace all idsujeto with 0

x=0
q1="begin"
if !sqli(q1)
	return
endif

q1="delete from sujetos where not isempleado and scode>=19500 and scode<20000"
if !sqli(q1)
	return
endif

SELE contacto
GO TOP
SCAN

	idsuj=0
	q1="select nextsecu('auxiliarescon_seq'::text, 'SUJETOS'::text) as idsujeto"
	if sqli(q1,'suj')
		idsuj=suj.idsujeto
	endif
	
	
	cate=5716

	ciud=3892
	
	prov=19
	
	sele contacto

	q1="insert into sujetos ( idsujeto, scode, isempleado, isproveedor, isproductor, iscliente, issocio, sname, ssri, "+;
				" smoney, sprov, sciudad, numord, tipcli) values "+;
				pal(idsuj) + al(rango)+ al(.f.) +al(.f.) +al(.f.) +al(.t.) +al(.f.) +;
				al(contacto) + al(contacto) +  ;
				al(2) +al(prov) +al(ciud) +al(1) +ual(cate)
				
	if sqli(q1)
		wait 'Registrado...' wind nowait
		replace contacto.idsujeto with idsuj
	else
		wait 'error' wind nowait
	endif
	
	rango=rango+1	
	sele contacto
ENDSCAN

do fintrans

wait 'Proceso concluido...' wind nowait

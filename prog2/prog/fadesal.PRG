*	Tabla de articulos

q1="select distinct iditem, iname, itag, costopro "+;
	"from varticulos  "
	
if !sqli(q1,'articulos') then
	return
endif


if !dg('BODE')
	return
endif

select iditem, iname, stock, costopro ;
from articulos i left join sbrame b on (i.itag=b.cod );
where stock>0  ;
group by iditem ;
into cursor saldos

x=0
if !sqli("begin;") then
	wait 'Problema de Comunicacion' wind nowait
	return
endif

q1="delete from saldosib where idsaldosi in "+;
		"(select idsaldosi from saldosi ) ;"

=sqli(q1) 

q1="delete from saldosi ;"

=sqli(q1) 

sele saldos
go top
do while !eof()

	item=iditem
	n=0
	
	q1="select nextval('saldosi_idsaldosi_seq') as ncod "
	
	if sqli(q1,'seq')
		sele seq
		go top
		n=ncod
	endif
	
	sele saldos
	wait alltrim(iname) wind nowait
		
	q1="insert into saldosi (idsaldosi, pdocode, iditem, inicial, precio,  "+;
			"inipen, fecsaldo) values "+pal(n)+al(iprd)+al(iditem)+al(stock)+;
			al(costopro)+al(0)+ual(finip)
	=sqli(q1)

	q1="insert into saldosib (idsaldosi, bodega, qtyini, qtypen) "+;
					"values "+pal(n)+al(2651)+al(stock)+;
					ual(0)
	=sqli(q1)
	
	sele saldos
	skip
enddo	

***********************************************************************
do fintrans
***********************************************************************

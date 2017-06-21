local cantexi, item
*	Tabla de articulos
q1="select distinct iditem, icode, iname, itag "+;
	"from varticulos  "
	
if !sqli(q1,'articulos') then
	return
endif


select codart, canexi, desart, ultcos, b_desp, local_1, local_2 ;
from ven001 ;
where len(alltrim(codart))>0 and canexi>0;
into cursor saldos1


x=0
if !sqli("begin;") then
	wait 'Problema de Comunicacion' wind nowait
	return
endif

q1="delete from saldosib where idsaldosi in "+;
		"(select idsaldosi from saldosi where pdocode="+alup(iprd)+") ;"

=sqli(q1) 

q1="delete from saldosi where pdocode="+alup(iprd)+";"

=sqli(q1) 

sele articulos 
go top
do while !eof()
	item=articulos.iditem
	n=0
	
	sele articulos 
	itagb=articulos.itag
	
	Select saldos1
	go top
	locate for ALLTRIM(saldos1.codart)=ALLTRIM(itagb)
	If Found() then
		cantexi=saldos1.canexi

		sele articulos 
		
		q1="select nextval('saldosi_idsaldosi_seq') as ncod "

		if sqli(q1,'seq')
			sele seq
			go top
			n=ncod
		endif
		
		sele articulos

		wait alltrim(str(articulos.icode))+' => '+alltrim(articulos.iname) wind nowait
			
		q1="insert into saldosi (idsaldosi, pdocode, iditem, inicial, precio, inipen) values "+;
					pal(n)+al(iprd)+al(articulos.iditem)+al(cantexi)+al(saldos1.ultcos)+ual(0)
				
		=sqli(q1)

*!*			bod=0
*!*			select bodegas
*!*			go top
*!*			locate for val(valorda)=1   &&val(saldos1.b_desp)  && Almacen Machala
*!*			if found()
*!*				bod=iddato
*!*			endif

		select saldos1
		if local_1>0
			q1="insert into saldosib (idsaldosi, bodega, qtyini, qtypen) "+;
							"values "+pal(n)+al(2651)+al(local_1)+ual(0)
			=sqli(q1)
		endif
		
		if local_2>0
			q1="insert into saldosib (idsaldosi, bodega, qtyini, qtypen) "+;
							"values "+pal(n)+al(5466)+al(local_2)+ual(0)
			=sqli(q1)
		endif
	Else
		cantexi=0
	EndIf
	
	sele articulos
	skip
	
	
enddo	

************
do fintrans
************

local cantexi, item
*	Tabla de articulos

q1="select distinct idsujeto, codigo as scode, nombre as sname "+;
	"from vsujetos  "
	
if !sqli(q1,'sujetos1') then
	return
endif

select numfac, tipo_fac, fecord, codprov, cliente, total, abonado, total-abonado as saldo ;
from ven004 ;
where tipo_fac='E' and total-abonado>0.01 ;
into cursor saldocli

select distinct s.*, c.idsujeto ;
from saldocli s left join sujetos1 c on alltrim(s.cliente)=alltrim(c.sname) ;
order by idsujeto into cursor cliente

Select * from cliente where idsujeto>0 into cursor cliente

x=0
if !sqli("begin;") then
	wait 'Problema de Comunicacion' wind nowait
	return
endif

*!*	q1="delete from saldosci ;"

*!*	=sqli(q1) 

sele CLIENTE
go top
SCAN FOR IDSUJETO>0

*	IDSUJETO1=CLIENTE.IDSUJETO
	
	wait alltrim(str(CLIENTE.IDSUJETO))+' => '+alltrim(CLIENTE.cliente) wind nowait
		
	q1="insert into saldosci (code, idsujeto, saldo, monto, pdocode, fecultpag, ctacon, fecven, cuota, referencia, fecha) values "+;
				pal(0)+al(cliente.idsujeto)+al(cliente.saldo)+al(cliente.saldo)+al(iprd)+al(cliente.fecord)+al(4198)+al(cliente.fecord)+al(cliente.saldo)+al(alltrim(cliente.cliente)+' - '+alltrim(cliente.codprov)+" - numfac "+alltrim(str(cliente.numfac)) )+ual(cliente.fecord)
			
	=sqli(q1)

	sele CLIENTE
	
EndScan	

************
do fintrans
************

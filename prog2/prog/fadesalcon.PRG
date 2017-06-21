select timov, i.codigo, i.fecha, c.fecha_ven, c.fecha_ult, i.procli, i.nomcli, i.ruc, ;
	i.cobrador, factura, i.vendedor, c.valor, c.saldo ;
from ini040 i left join cci010 c on (i.tipo+i.codigo=c.codigo) ;
where !i.anulada and saldo>0 and (timov='FC' or timov='NV') ;
order by i.procli ;
into cursor saldo

x=0
if !sqli('begin;')
	return
endif

sele saldo
go top
do while !eof()
	r=iif(len(alltrim(ruc))=10,"scedula="+alup(ruc),;
	  iif(len(alltrim(ruc))=13,"sruc="+alup(ruc),;
	  	  alup(.t.)))

	nom=strtran(nomcli,"'","")
	nom=strtran(nomcli,'"',"")  	
	  
	q1="select idsujeto from sujetos where "+r+" or sname like '%"+alltrim(upper(nom))+"%' or ssri like '%"+alltrim(upper(nom))+"%'"
	if sqli(q1,'sujeto')
		if reccount('sujeto')=0
			=messagebox(alltrim(saldo.nomcli)+' no existe')
		else
			sele saldo
			q1="insert into saldosci (idsujeto, saldo, monto, pdocode, fecultpag, ctacon, fecven, "+;
				"referencia, cobrador) values "+;
				pal(sujeto.idsujeto)+al(saldo)+al(saldo)+al(iprd)+al(fecha_ult)+al(4198)+al(fecha_ven)+;
				al(timov+' : '+alltrim(factura)+' por '+alltrim(str(valor,10,2)))+ual(cobrador)
			=sqli(q1)
				
		endif
	else
		wait 'error' wind
	endif
	sele saldo
	skip
enddo

do fintrans
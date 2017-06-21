Local cuitem1, curMXbod

q1=" Select iditem, itag, icode, iname from items "

IF !sqli(q1,"curLITE") then
	return .f.
ENDIF	

Select bramesa15

 SELECT distinct i.icode as codigo, i.iname as descripda, c.iname, i.inicial15, i.costopro, ;
	c.iditem, c.itag, c.icode   ;
 FROM  bramesa15 i INNER JOIN curLITE c ON (val(i.icode)=c.icode ) ;
 where val(i.inicial15)>0 INTO CURSOR bodega1

Select * from bodega1 order by iditem into cursor bodega2

Select * from bodega1 group by iditem order by iditem into cursor bodega4

=dg('BODE')

Select bodega4
go top

Scan
	Select bodega4
	
	cuitem1=bodega4.iditem

	q1="insert into saldosi ( pdocode,  iditem, inicial, precio, cospro, fecsaldo) "+;
		"values "+pal(54)+al(bodega4.iditem)+al(bodega4.inicial15)+ al(bodega4.costopro)+al(bodega4.costopro)+ual('2015-01-01')
		
	if !sqli(q1) then
		return
	endif
	
	Select * from bodega2 where iditem=bodega4.iditem into cursor bodega3
	
	Select Bodega3
	go top
	Scan
		Select bodegas
		go top

		bodi1=2651

		q1="select max(idsaldosi) as mxidsaldosi from saldosi"
		=sqli(q1,"mxsaldos1")
		
		Select mxsaldos1
		go top
		
		curMXbod=mxsaldos1.mxidsaldosi
		
		Select Bodega3
		
		if (val(Bodega3.inicial15))>0
			q1="insert into saldosib (idsaldosi, bodega, qtyini) values "+;
					pal(curMXbod)+ al(bodi1) + ual(Bodega3.inicial15)
					
			if !sqli(q1) then
				return
			endif

		endif

		Select Bodega3

	EndScan
	
	
	Select bodega4
EndScan


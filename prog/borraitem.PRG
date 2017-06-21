wait 'Seleccionado items' wind nowait

SELECT CODIGO, NOMBRE FROM ARTICULOS WHERE GRUPO<>'045.' AND GRUPO<>'038.' AND CODIGO NOT IN ;
(SELECT CODIGO FROM PVTA_MOV D, PVTA_CAB P WHERE D.ID=P.ID AND TTOD(P.FECHA)>=DATE(2008,10,01))  ;
order by nombre ;
into cursor borrar

sele borrar
go top
do while !eof()
	q1="select iditem from items where itag="+alup(codigo)+" and iditem not in (select iditem from detadoci) "
	if sqli(q1,'item')
		sele item
		if reccount('item')=1
			wait 'borrando '+alltrim(borrar.nombre) wind nowait
			x=0
			=sqli("begin")

			q1="delete from preciosv where iditem="+alup(item.iditem)
			=sqli(q1)

			q1="delete from items where iditem="+alup(item.iditem)
			=sqli(q1)
			=fintransq()
		endif
	endif
	sele borrar
	skip
enddo
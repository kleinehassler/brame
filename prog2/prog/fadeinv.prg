IMPORT FROM c:\brame\migrar\i301.xls TYPE XL8 SHEET "_I301"
sele i301
go top
delete
select  a as codigo, c as unidad, d as marca, f as clase, val(g) as costopro, h='V' as isiva,  j as lista, n as minimo, ;
	o as maximo, v as nombre, w as lista2, x as lista3, y as lista4 ;
from i301 ;
into table \brame\migrar\articulos

delete for empty(codigo) and empty(nombre)

replace marca with 'NINGUNA' for empty(marca)
replace unidad with 'S/N' for empty(unidad)
replace all nombre with strtran(nombre,"'",'')
replace all marca with strtran(marca,"'",'')
replace all codigo with strtran(codigo,"'",'')

SELECT * FROM ARTICULOS ;
ORDER BY NOMBRE ;
INTO CURSOR ARTIC

select codigo, descripcion from clases into cursor grupos

x=0

=sqli("begin;")

q1="delete from preciosv "
=sqli(q1)

q1="delete from items where itipo=1"
=sqli(q1)

q1="delete from detagru where iddato<>5717 and idgrupo=44"
=sqli(q1)

q1="delete from detagru where iddato<>5819 and idgrupo=43"
=sqli(q1)

q1="ALTER SEQUENCE auxiliarescon_seq RESTART WITH 7000"
=sqli(q1)

q1="ALTER SEQUENCE detagru_iddato_seq RESTART WITH 12700"
=sqli(q1)

if !fintransq()
	wait 'error' wind nowait
	return
endif

wait 'importanto Tipos' wind at 2,2 nowait

sele grupos
GO TOP
do while !eof()
	q1="insert into detagru (valorda, descripda, idgrupo) values "+;
			pal('PT')+al(descipcion)+ual(44)
				
	=sqli(q1)
		
	skip
enddo

select * from marcas into cursor marcas2

wait 'importanto Marcas' wind at 2,2 nowait

sele marcas2
go top
do while !eof()
	q1="insert into detagru (descripda, valorda, idgrupo) values "+;
			pal(descipcion)+al(codigo)+ual(43)
	=sqli(q1)
	sele marcas2
	skip
enddo

IF !dg('MAIT')
	RETURN
ENDIF

IF !dg('TIAR')
	RETURN
ENDIF

IF !dg('MEDI')
	RETURN
ENDIF


icde=0
sele ARTIC
go top
do while !eof() 
	
	wait 'importanto '+alltrim(nombre) wind at 2,2 nowait
	
	ICODE=0
	q1="select max(icode)+1::int2 as maxcod from items ;"
	if sqli(q1,'numcod') then
		sele numcod
		icode=iif(isnull(maxcod),1,maxcod)
	endif
	
	idi=0
	q1="select 	nextsecu('auxiliarescon_seq'::text, 'ITEMS'::text) as idi"
	if	sqli(q1,'nit')
		sele nit
		if reccount('nit')>0
			idi=nit.idi
		endif
	endif

	ntip=5717
	sele grupos
	go top
	locate for alltrim(codigo)=alltrim(artic.clase)
	if found()
		sele tipart
		go top
		locate for alltrim(descripda)=alltrim(grupos.descipcion)
		if found()
			ntip=tipart.iddato
		endif
	endif

	marc=5819
	sele marcasit
	go top
	locate for alltrim(valorda)=alltrim(ARTIC.marca)
	if found()
		marc=marcasit.iddato
	endif	
	
	uni=7992
	
	sele ARTIC
	
	q1="insert into items (artgrupo, artmarca, issell, isbuy, isreturn, itipo, iname, iunidad, iunidad2, "+;
			" isiva, itag, icode, stockmin, stockmax, imoney, artpeso, costopro, artcosto, isdesc, iditem) values "+;
			pal(ntip)+al(marc)+al(.t.)+al(.t.)+al(.f.)+al(1)+al(nombre)+al(uni)+al(uni)+;
			al(artic.isiva)+al(codigo)+al(ICODE)+al(minimo)+al(maximo)+al(2)+;
			al(0)+al(costopro)+al(costopro)+al(.t.)+ Ual(idi)
	
	if !sqli(q1)
		=messagebox('error en articulo: '+alltrim(artic.nombre),0,empresa)
	endif
	
	sele ARTIC
	q1="insert into preciosv (iditem, iddato, preciov) values "+;
			pal(idi)+al(5716)+ual(lista)
	=sqli(q1)

	q1="insert into preciosv (iditem, iddato, preciov) values "+;
			pal(idi)+al(12577)+ual(lista2)
	=sqli(q1)

	q1="insert into preciosv (iditem, iddato, preciov) values "+;
			pal(idi)+al(12575)+ual(lista3)
	=sqli(q1)

	q1="insert into preciosv (iditem, iddato, preciov) values "+;
			pal(idi)+al(12576)+ual(lista4)
	=sqli(q1)

	skip
enddo


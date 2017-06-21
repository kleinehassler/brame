if used('precios')
	sele precios
else
	sele 0
	use precios
endif

q1="delete from preciosv "
=sqli(q1)

GO TOP

tipoc=5716

do while !eof()
	
	wait 'registrando precios'+alltrim(nombre) wind nowait
	q1="select iditem from items where itag="+alup(codigo)
	if sqli(q1,'item')
		if item.iditem>0

			sele precios
			q1="insert into preciosv (iditem, iddato, preciov) values "+;
			pal(item.iditem)+al(5716)+ual(preaund)
			if sqli(q1)
				wait 'registrado.' wind AT 1,1 nowait
			endif
		endif
	endif
	sele precios
	
	skip
enddo


IF !USED('fai100')
	USE fai100.dbf IN 0 SHARED
ENDIF
SELEC DISTINC nombre, codigo FROM fai100 WHERE !EMPTY(nombre) INTO CURSOR Ciudades

q1="delete from detagru where idgrupo="+alup(23)+" and iddato not in (select idciudad from workstations) "+;
						+" and iddato not in (select sciudad from sujetos) "
if !sqli(q1)
	return
endif

SELE Ciudades
GO TOP
scan
	if UPPER(nombre)<>'MACHALA'
		q1="insert into detagru (idgrupo, descripda, valorda) values "+pal(23)+al(upper(nombre))+ual(codigo)
		=sqli(q1)
	ENDIF
endscan
WAIT 'PROCEDO CONCLUIDO...' WIND NOWAIT
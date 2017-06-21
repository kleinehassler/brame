if !dg('CUCO')
	return
endif

if used('plancta')
	sele plancta
else
	sele 0
	use plancta
endif


x=0
sele plancta
replace all codigo with alltrim(codigo)

go top
if !sqli("begin;")
	return
endif

q1="delete from plancuenta ;"
if !sqli(q1)
	return
endif

do while !eof()
	l=len(alltrim(CODIGO))
	c=alltrim('0'+substr(codigo,1,1))+alltrim(substr(codigo,3,2))+alltrim(substr(codigo,6,2))+alltrim(substr(codigo,9,2))+;
		alltrim(substr(codigo,12,2))+alltrim(substr(codigo,15,2))+substr('00000000000000000000',1,20-l)

	sele ctasctbs
	go top
	locate for alltrim(descripda)=alltrim(plancta.nombre)
	if found()
		sele plancta
		q1="insert into plancuenta (plancod, idcuenta, plannivel, plantype, auxcode, pdocode) values "+;
			pal(c)+al(ctasctbs.iddato)+al( 	iif(l=2,1,iif(l=4 or l=5,2,iif(l=7 or l=8,3,iif(l=10 or l=11,4,iif(l=13 or l=14,5,6) )))))+;
			al(iif(DEGRUPO='S','G','M'))+al(iif(DEGRUPO='S',NULL,'S'))+ual(54)
		=sqli(q1)
		*wait q1 wind
		
	else
		idd=0
		q1="select 	nextsecu('detagru_iddato_seq'::text, 'DETAGRU'::text) as idd"
		if	sqli(q1,'nit')
			sele nit
			if reccount('nit')>0
				idd=nit.idd
			endif
		endif
		
		sele plancta
		q1="insert into detagru (iddato, descripda, idgrupo) values "+;
			pal(idd)+al(upper(NOMBRE))+ual(21)
		=sqli(q1)


		q1="insert into plancuenta (plancod, idcuenta, plannivel, plantype, auxcode, pdocode) values "+;
			pal(c)+al(idd)+al( 	iif(l=2,1,iif(l=4 or l=5,2,iif(l=7 or l=8,3,iif(l=11 or l=14,4,iif(l=13 or l=14,5,6) )))))+;
			al(iif(DEGRUPO='S','G','M'))+al(iif(DEGRUPO='S',NULL,'S'))+ual(54)

		=sqli(q1)
		*wait q1 wind
		if !dg('CUCO')
			x=1
			exit
		endif
	endif
	sele plancta
	skip
enddo

do fintrans
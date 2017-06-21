if !dg('CIUD')
	WAIT 'ERROR EN CIUDADES' WIND NOWAIT
	RETURN
ENDIF

if !dg('TICL')
	WAIT 'ERROR EN TIPOS DE CLIENTES' WIND NOWAIT
	RETURN
ENDIf

SELECT distin m.codalu, m.nomalu, m.nummatr, m.apoderado, m.dirapo, cedapo, m.tela, a.telalu, ;
a.telalu2, a.diralu, a.nompad, a.dirp, c.numero, c.paralelo, c.nomsal, c.jornada ;
FROM m2009 m LEFT JOIN alumno a ON (m.codalu=a.codalu) ;
			 LEFT JOIN curso c ON (m.codcur=c.codcur) ;
WHERE c.periodo='2009' ;
ORDER BY m.codalu ;
into table estudiante

select nomsal, paralelo, jornada ;
from curso ;
where periodo='2009' ;
into table tmpcur

sele tmpcur
go top

idgru=242
scan
	q1="select iddato from detagru where descripda="+alup(alltrim(nomsal)+' '+alltrim(paralelo))
	if sqli(q1,'loccur')
		if reccount('loccur')=0
			sele tmpcur
			q1="insert into detagru (idgrupo, descripda, valorda) values "+;
				pal(242)+al(alltrim(nomsal)+' '+alltrim(paralelo))+ual(jornada)
			=sqli(q1)
		endif
	else
		return
	endif
endscan

if !dg('CURS')
	WAIT 'ERROR EN CURSOS' WIND NOWAIT
	RETURN
ENDIF

SELE estudiante
replace all telalu 	with strtran(telalu,'-','')
replace all tela 	with strtran(tela,'-','')
replace all telalu2 with strtran(telalu2,'-','')
replace all cedapo 	with strtran(cedapo,'-','')

replace all nomalu		with strtran(nomalu,"'","")
replace all nomalu		with strtran(nomalu,'"',"")
replace all apoderado	with strtran(apoderado,"'","")
replace all apoderado	with strtran(apoderado,'"',"")
replace all dirapo		with strtran(dirapo,"'","")
replace all dirapo		with strtran(dirapo,'"',"")
replace all diralu		with strtran(diralu,"'","")
replace all diralu		with strtran(diralu,'"',"")
replace all nompad		with strtran(nompad,"'","")
replace all nompad		with strtran(nompad,'"',"")

GO TOP
SCAN
	ncodigo=0
	q1="select max(scode)::int4 as coda from sujetos "
	if sqli(q1,'coda')
		sele coda
		ncodigo=iif(isnull(coda.coda),1,coda.coda+1)
	endif
	
	cate=3254
	ciud=4147
		
	sele cursos
	go top
	locate for 	alltrim(descripda)=alltrim(estudiante.nomsal)+' '+alltrim(estudiante.paralelo) and ;
				alltrim(valorda)=alltrim(estudiante.jornada)
	if found()
		idcurso=cursos.iddato
	else
		idcurso=0
	endif
	
	sele estudiante

	q1="select scode from sujetos where sname like '"+alltrim(upper(nomalu))+"' "
	if sqli(q1,'sujeto')
		if reccount('sujeto')=0
			sele estudiante
			q1="insert into sujetos ( scode, isempleado, isproveedor, isproductor, iscliente, issocio, sname, ssri, "+;
						" saddr, saddr2, stelf, sfax, scel, smoney, sprov, sciudad, stag, numord, tipcli, scedula, idcurso) values "+;
						pal(ncodigo) +al(.f.) +al(.f.) +al(.f.) +al(.t.) +al(.f.) +;
						al(alltrim(nomalu)) + al(iif(isnull(nompad) or empty(nompad),apoderado,nompad)) + al(dirp) +;
						al(diralu) + al(tela)+ al(telalu) +al(telalu2) +al(2) +al(19) +al(ciud) +al(nummatr) +al(1) +al(cate)+;
						al(cedapo) + ual(idcurso)
			if sqli(q1)
				wait 'Registrado...' wind nowait
			endif
		else
			sele estudiante
			q1="update sujetos set idcurso="+alup(idcurso)+;
				" where iscliente and sname like '"+alltrim(upper(nomalu))+"' "
			
			if sqli(q1)
				wait 'Actualizado...' wind nowait
			endif
				
		endif
	else
		wait 'error' wind nowait
	endif
		
	sele estudiante
ENDSCAN

wait 'Proceso concluido...' wind nowait

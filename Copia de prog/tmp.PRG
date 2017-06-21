      
ncod=0
tarc=0
nite=13989
fd=date(2012,01,01)
fh=date(2012,10,05)
tipcalcos=1

*!*	ncod 	==> codigo de documento
*!*	tarc 	==> tipo de articulo
*!*	nite	==> codigo de item (iditem)
*!*	fd		==> fecha inicio de actualización de documento
*!*	fh		==> fecha finalizacion de actualización de documento

local pdoc, td, pu, ci, cf, cdoc, f, s, sfin, pua, pup, ffi, fuc


cf=0
ci=0
f=iif(!empty(fh) and !isnull(fh),fh,hoy)
fp=iif(!empty(fd) and !isnull(fd),fd,finip)
sfin=.f.

if tipcalcos=1
	s=alup(.t.)

	if !empty(ncod) and !isnull(ncod)
		q1="select isaccount, swkar, isdevo, swconta, fecha, istransfer "+;
			"from vdocusmall "+;
			"where code="+alup(ncod)
		if sqli(q1,'dpri')
			sele dpri
			go top
			if reccount()=0
				use
				return .f.
			endif
			
			if !swkar then
				return .t.
			endif

			if !swconta 
				return .t.
			else
				if !isaccount
					return .t.
				endif
			endif
			f=dpri.fecha
			fp=dpri.fecha
		endif
		s="iditem in (select iditem from detadoci where code="+alup(ncod)+") "
	else
		if !empty(tarc)
			s="iditem in (select iditem from items where artgrupo="+alup(tarc)+") "
		else
			if !empty(nite)
				s="iditem="+alup(nite)+" "
			endif
		endif
	endif

	fuc=hoy		
*!*		q1="select max(fecha) as fecha "+;
*!*			"from costeoinv "

*!*		if sqli(q1,'fultcost')
*!*			if reccount('fultcost')=1
*!*				fuc=iif(isnull(fultcost.fecha) or empty(fultcost.fecha),finip,fultcost.fecha)
*!*			endif
*!*		endif


	q1="select max(fecsaldo) as fecsaldo "+;
		"from saldosi "+;
		"where fecsaldo<="+alup(fp)

	ffi=finip		
	if sqli(q1,'fmaxsal')
		if reccount('fmaxsal')<>0
			ffi=iif(isnull(fmaxsal.fecsaldo) or empty(fmaxsal.fecsaldo),ffi,fmaxsal.fecsaldo)
		endif
	endif
		
	* Saldos Iniciales
	q1="select iditem, sum(inicial) as inicial, max(precio) as precio "+;
		"from saldosi "+;
		"where fecsaldo="+alup(ffi)+" and "+s+;
		"group by iditem;"
	if !sqli(q1,'saldosi') then
		return .f.
	endif

	* Calculo  de Ingresos en este periodo
	q1="select fecha, iditem, artcosto, descuento, artcosto as punitario, iname, icode, qty, isdevo, "+;
			"d.code, d.istransfer  "+;
		"from vdocui i left join vdocusmall d on (d.code=i.code) "+;
		"where not d.istransfer and d.swkar and fecha>"+alup(ffi)+" and fecha<="+alup(f)+" and tipsaldo=2 and "+fcont+" and "+;
			"not isanulado and "+s+;
			" order by iditem, fecha;"
	if !sqli(q1,'ingresos') then
		return .f.
	endif

	? s
	* Tabla de Items
	q1="select iditem, icode, iname, artcosto, costopro "+;
		"from items "+;
		"where itipo=1 and "+s+" order by iname;"
		
	if !sqli(q1,'articulos') then
		return .f.
	endif

	* Saldos Iniciales e Ingresos
	sele articulos
	go top
	do while !eof()
		x=0
		
		wait 'Actualizando '+alltrim(articulos.iname) wind at 3,1 time 0.3

		if !sqli("begin;") then
			exit
		endif

		sele saldosi
		go top
		locate for iditem=articulos.iditem
		if found()
			pu	=iif(isnull(precio) or empty(precio),0, precio)
			pua	=iif(isnull(precio) or empty(precio),0, precio)
			ci	=iif(isnull(inicial),0,inicial)
		else
			pu	=0
			pua	=0
		endif

		fi=fp
		
		sele ingresos
		set filter to
		set filter to iditem=articulos.iditem
		go top
		
		if !eof()
			if pu=0 or pua=0
				pu	=ingresos.artcosto
				pua	=ingresos.artcosto
			else
				pup=ingresos.artcosto
			endif
		else
			if pu=0
				pu=iif(isnull(articulos.artcosto) or empty(articulos.artcosto), articulos.costopro, articulos.artcosto)
			endif
		endif
		
		
		? round(iif(empty(pu) or isnull(pu),iif(empty(pua) or isnull(pua),iif(empty(pup) or isnull(pup),0,pup),pua) ,pu),5)
		
		do while !eof()
			*	Actualizacion solo del hasta la fecha definida, caso contrio no actualizar
			
			if fecha>=fp and fecha<=f 		&&and fecha>fuc
				*	Actualizacion de Facturas
				q1="update detadoci set artcosto="+alup(round(iif(empty(pu) or isnull(pu),iif(empty(pua) or isnull(pua),iif(empty(pup) or isnull(pup),0,pup),pua) ,pu),5))+;
					" where iditem="+alup(articulos.iditem)+" and code in "+;
					"(select code "+;
					" from vdocusmall "+;
					" where tipsaldo=1 and "+fcont+ " and not isdevo and not istransfer "+;
					" and not isanulado and fecha<"+;
						alup(fecha)+" and fecha>="+alup(fi)+")"
				if !sqli(q1)
					wait 'Error en actualización' wind nowait
				endif
				
				
				? round(iif(empty(pu) or isnull(pu),iif(empty(pua) or isnull(pua),iif(empty(pup) or isnull(pup),0,pup),pua) ,pu),5)
					
				*		Actualizacion de Devoluciones de ventas
				if ingresos.isdevo
					q1="update detadoci set artcosto="+alup(round(iif(empty(pu) or isnull(pu),iif(empty(pua) or isnull(pua),iif(empty(pup) or isnull(pup),0,pup),pua) ,pu),5))+;
						" where iditem="+alup(articulos.iditem)+" and code="+;
						alup(ingresos.code) 
					if !sqli(q1)
						wait 'Error en actualización' wind nowait
					endif
				endif

			endif
			
			*	Calculo de Egresos en este periodo, los mismos documentos q se actualizan
			q1="select sum(qty) as qty  "+;
				"from detadoci i left join vdocusmall d on (d.code=i.code) "+;
				"where not istransfer and tipsaldo=1 and swkar and "+fcont+" and not isanulado and "+;
						"iditem="+alup(articulos.iditem)+;
						" and pdocode="+alup(iprd)+" and fecha<"+alup(fecha)+" and fecha>="+alup(fi)
			if sqli(q1,'egr')
				ce=iif(isnull(egr.qty),0,egr.qty)
				
				? ce
				
			*	No puede haber mas egresos de los q hay			
				if ci-ce<0 
					if (empty(pu) or isnull(pu)) and (empty(pua) or isnull(pua)) and (empty(pup) or isnull(pup))
						x=1
					endif
				else
					if !ingresos.isdevo and !ingresos.istransfer
						pu=iif(ci-ce+ingresos.qty>0, ;
								((ci-ce)*pu + (ingresos.qty*ingresos.artcosto) -ingresos.descuento)/((ci-ce)+ingresos.qty),;
								ingresos.artcosto)			
						?' calculo: '+alup(pu)
						if pu<=0
							pu=pua
						else
							pua=pu
						endif
						
						sele ingresos
						skip
						if !eof()
							pup=artcosto
						endif
						skip -1
						
						? ' pu: '+alup(pu)
					endif
				endif
				
				ci=ci-ce+ingresos.qty

			endif

			sele ingresos
			fi=fecha
			skip
		enddo
		
		* Si tuvieramos otros egresos despues del ultimo ingreso
		q1="select sum(qty) as qty  "+;
			"from detadoci i left join vdocusmall d on (d.code=i.code) "+;
			"where tipsaldo=1 and "+fcont+" and not isanulado and not isdevo and "+;
					"iditem="+alup(articulos.iditem)+;
					" and pdocode="+alup(iprd)+" and fecha>="+alup(fi)+" and fecha<="+alup(f)

		if sqli(q1,'egr')
			cf=iif(isnull(egr.qty),0,egr.qty)

			ci=ci-cf
		endif

		*	Actualizacion de facturas
		if fi<=f &&and fi>fuc 
			q1="update detadoci set artcosto="+alup(pu)+;
					" where iditem="+alup(articulos.iditem)+" and code in "+;
						"(select code "+;
						" from vdocusmall "+;
						" where tipsaldo=1 and swkar and "+fcont+" and not isdevo and not istransfer "+;
						" and not isanulado and fecha>="+alup(fi)+" and fecha<="+alup(f)+")"
			=sqli(q1)
		endif	
		
		&& Actualizar el costo del kardex
		if f=hoy
	*!*			q1="update items set saldocon="+alup(ci)+", artcosto="+alup(pu)+;
	*!*					" where iditem="+alup(articulos.iditem)+";"
	*!*			=sqli(q1)
		endif	
		**********************************************************************
		if !fintransq() then
			=messagebox('Error en '+alltrim(str(articulos.icode))+' '+;
			 alltrim(articulos.iname),0,empresa)
		endif
		**********************************************************************
		
		sele articulos
		
		skip
	enddo

	wait clear 

	return .t.
else
	
	s=alup(.t.)
	
	if !empty(ncod) and !isnull(ncod)
		q1="select isaccount, swkar, isdevo, swconta, fecha, istransfer "+;
			"from vdocusmall "+;
			"where code="+alup(ncod)
		if sqli(q1,'dpri')
			sele dpri
			go top
			if reccount()=0
				use
				return .f.
			endif
			
			if !swkar then
				return .t.
			endif

			if !swconta 
				return .t.
			else
				if !isaccount
					return .t.
				endif
			endif
			f=dpri.fecha
			fp=dpri.fecha
		endif
		s="iditem in (select iditem from detadoci where code="+alup(ncod)+") "
	else
		if !empty(tarc)
			s="iditem in (select iditem from items where artgrupo="+alup(tarc)+") "
		else
			if !empty(nite)
				s="iditem="+alup(nite)+" "
			endif
		endif
	endif


	* Tabla de Items
	q1="select iditem, icode, iname, artcosto, costopro "+;
		"from items "+;
		"where itipo=1 and "+s+" order by iname;"
		
	if !sqli(q1,'articulos') then
		return .f.
	endif

	q1="select distinct cospro, iditem, min(fecsaldo) as fecsaldo "+;
				" from saldosi "+;
				" where fecsaldo>"+alup(fp)+;
				" group by 1,2 "+;
				" order by 2,3 "
	
	if !sqli(q1,'fsaldos')
		return .f.
	endif		

	wait 'Pprocesando.... ' wind nowait	

	sele fsaldos
	
	sele articulos
	go top
	do while !eof()

		wait 'Actualizando '+alltrim(articulos.iname) wind at 3,1 time 0.1

		sele fsaldos
		go top
		locate for iditem=articulos.iditem
		if found()
			x=0
			*	Actualizacion de Facturas
			q1="update detadoci set artcosto="+alup(fsaldos.cospro)+;
				" where iditem="+alup(fsaldos.iditem)+" and code in "+;
				"(select code "+;
				" from vdocusmall "+;
				" where tipsaldo=1 and "+fcont+ " and not isdevo and not istransfer "+;
				" and not isanulado and fecha>="+alup(fp)+" and fecha<="+alup(f)+")"
			
			if !sqli(q1)
				wait 'Error en actualización' wind nowait
			endif

			q1="update detadoci set artcosto="+alup(fsaldos.cospro)+;
				" where iditem="+alup(fsaldos.iditem)+" and code in "+;
					" (select code "+;
						" from vdocusmall "+;
						" where isdevo and swkar and fecha>="+alup(fp)+" and fecha<="+alup(f)+;
							" and tipsaldo=2 and "+fcont+" and "+;
							"not isanulado )"
			if !sqli(q1)
				wait 'Error en actualización' wind nowait
			endif

		endif
		if !fintransq() then
			=messagebox('Error en '+alltrim(str(articulos.icode))+' '+;
			 alltrim(articulos.iname),0,empresa)
		endif

		sele articulos
		skip
	enddo
	wait clear 

	return .t.

endif

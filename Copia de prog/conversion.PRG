select alltrim(a) as codigo, c as unidad, d as marca, e as modelo, f as clase, val(g) as costopro, ;
	h='V' as iva, val(n) as minimo, val(o) as maximo, v as nombre, val(j) as lista, val(w) as lista2, ;
	val(x) as lista3, val(y) as lista4, ac as itag ;
from i301 into table \brame\migrar\articulos

sele articulos
go top
delete
pack

select distinct a as codigo, b as descripcion from i105 into table \brame\migrar\zonasM
sele zonasM
go top
delet
pack

select a as codigo, b as descipcion from i302 into table \brame\migrar\marcas
sele marcas
go top
delet
pack

select a as codigo, b as descipcion from i304 into table \brame\migrar\clases
sele clases
go top
delete
pack

*	Ojo Email.
select alltrim(a) as codigo, b as ruc, c as contacto, f as ciudad, i as gerente, s as saddr, ;
	v as nombre, w as telefono, x as telefono2, aa as ctaban, ab as email ;
from i401 into table \brame\migrar\proveedores
BROWSE LAST
go top
dele
pack


select alltrim(a) as codigo, b as nombre, c as ruc, d as direc, e as fono1, f as fono2, g as fax, bb as fec_ing, ;
	h as email, i as representa, l as activo, m as ciudad, r as numcasa, s as gerente, iif(an='F',.f.,.t.)  as credito, ;
	iif(ao='F',.f.,.t.) as cheque, iif(ap='F',.f.,.t.) as chepos, iif(aq='F',.f.,.t.) as tc, ba as tipnegocio, ;
	bc as zona, be as ssri, bd as tamano ;
from i501 into table \brame\migrar\clientes



i105=> zonas
i601=> plan de cuentas

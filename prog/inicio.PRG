LOCAL ruta
ruta = SYS(2003) + '\bdd;' +  ;
       SYS(2003) + '\prog;' +  ;
       SYS(2003) + '\form;' +  ;
       SYS(2003) + '\report;' +  ;
       SYS(2003) + '\varios'
SET PATH TO &ruta
ruta = SYS(2003) + '\tempo'
SET DEFA TO &ruta
SET PROCEDURE TO PROCEDI.fxp, FCONSULTAS.fxp;
ADDITIVE
SET CLASSLIB TO botones ADDITIVE
SET CLASSLIB TO general ADDITIVE
SET SYSMENU TO DEFAULT
SET DATE TO DMY
SET CENTURY ON
SET SAFETY OFF
SET EXCLUSIVE OFF
SET TALK OFF
SET DELETED ON
SET POINT TO '.'
SET SEPARATOR TO ','
SET DECIMALS TO 6
ON ERROR DO MANEJO_ERR
ON KEY LABEL F9 DO VERROR
ON KEY LABEL ALT+C !/N CALC
PUBLIC hoy, ide, idu, dtu, idse,  ;
       mascara1, mascara2,  ;
       mascara3, mascarae, idb,  ;
       cmoneda, smone, swpreiva,  ;
       idagen, confbod, ingcaj,  ;
       escaja, ccaja, nconec,  ;
       final, cadenaorig,  ;
       cadenadest, empresa,  ;
       empresa2, usuario,  ;
       isabierto, isactual, cta,  ;
       fta, cfa, iprd, agencia,  ;
       ciudad, finip, ffinp,  ;
       fonoemp, rucemp, diremp,  ;
       idws, equipo, nivp,  ;
       modulok, nombdd,  ;
       idsecudocy, idautosriy,  ;
       tippery, iddocuy, numy,  ;
       ctasnums, doccodey, cgru,  ;
       tlbrdesktop, q1, basedd, x,  ;
       consindes, calsalbod,  ;
       actcosanu, printofil,  ;
       postoobar, codsriiva,  ;
       swkeycon, fcont, swkarneg,  ;
       swbloctadoc, ismaster,  ;
       alectivo, idcajero,  ;
       confimp, printasic,  ;
       tipcalcos, sruccontador,  ;
       contador, vpctaxcob
idsecudocy = 0
idautosriy = 0
tippery = 0
iddocuy = 0
numy = 0
doccodey = 0
ctasnums = 0
cadenaorig = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890'
cadenadest = '2468013579AEIOUBCDFGHJKLMNPQRSTVWXYZ'
isabierto = .T.
isactual = .T.
hoy = {}
idu = 678
ide = 1
idb = 2651
idse = 56
empresa = ''
usuario = ''
smone = ''
dtu = ''
cmoneda = 0
iprd = 51
mascara1 = '999,999.99'
mascara2 = '999,999,999.99'
mascara3 = '99,999.99999'
mascarae = '9,999,999'
finip = DATE(2005, 01, 01)
ffinp = DATE(2005, 12, 31)
final = 0
cta = 0
fta = tipolet('Times New Roman')
cfa = 14215660
agencia = ''
ciudad = ''
fono = ''
idws = 7
nivp = 0
equipo = ''
modulok = 'ADQ'
nombdd = ALLTRIM(nomunico())
cgru = ''
q1 = ''
basedd = ''
x = 0
consindes = .F.
calsalbod = .F.
actcosanu = .F.
printofil = .F.
postoobar = 0
codsriiva = 2
swkeycon = .F.
swkarneg = .F.
swpreiva = .F.
idagen = 0
confbod = .F.
ingcaj = .F.
swbloctadoc = .F.
ismaster = .F.
alectivo = ''
idcajero = 0
escaja = .F.
confimp = .T.
printasic = .F.
ccaja = ''
diremp = ''
empresa2 = ''
tipcalcos = 1
sruccontador = ''
contador = ''
ENDPROC
**

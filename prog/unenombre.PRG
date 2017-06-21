LOCAL idsuj1, idsuj2
idsuj1 = 0
idsuj2 = 0
q1 = "select count(scode), sname " +  ;
     "from sujetos " +  ;
     "where char_length(sname)>=30 " +  ;
     "group by sname having count(scode)>=2"
IF  .NOT. sqli(q1, 'ced_rep')
     RETURN
ENDIF
SELECT ced_rep
GOTO TOP
SCAN
     WAIT WINDOW NOWAIT  ;
          'Procesando ' +  ;
          ALLTRIM(sname)
     q1 = "select distinct idsujeto " +  ;
          "from sujetos " +  ;
          "where sname=" +  ;
          alup(ced_rep.sname)
     IF  .NOT. sqli(q1, 'sujtra')
          LOOP
     ELSE
          IF RECCOUNT('sujtra') <  ;
             2
               LOOP
          ENDIF
     ENDIF
     SELECT sujtra
     GOTO TOP
     idsuj1 = idsujeto
     SKIP
     idsuj2 = idsujeto
     x = 0
     IF  .NOT. sqli('begin')
          RETURN
     ENDIF
     q1 = "update documents set sujter=" +  ;
          alup(idsuj1) +  ;
          " where  sujter=" +  ;
          alup(idsuj2)
     = sqli(q1)
     q1 = "update documents set seller=" +  ;
          alup(idsuj1) +  ;
          " where  seller=" +  ;
          alup(idsuj2)
     = sqli(q1)
     q1 = "update documents set casher=" +  ;
          alup(idsuj1) +  ;
          " where  casher=" +  ;
          alup(idsuj2)
     = sqli(q1)
     q1 = "update documents set chofer=" +  ;
          alup(idsuj1) +  ;
          " where  chofer=" +  ;
          alup(idsuj2)
     = sqli(q1)
     q1 = "update documents set usercaja=" +  ;
          alup(idsuj1) +  ;
          " where  usercaja=" +  ;
          alup(idsuj2)
     = sqli(q1)
     q1 = "update documents set idugra=" +  ;
          alup(idsuj1) +  ;
          " where  idugra=" +  ;
          alup(idsuj2)
     = sqli(q1)
     q1 = "update documents set cliente=" +  ;
          alup(idsuj1) +  ;
          " where  cliente=" +  ;
          alup(idsuj2)
     = sqli(q1)
     q1 = "update accdoc set idsujeto=" +  ;
          alup(idsuj1) +  ;
          " where idsujeto=" +  ;
          alup(idsuj2)
     = sqli(q1)
     q1 = "update saldosci set idsujeto=" +  ;
          alup(idsuj1) +  ;
          " where idsujeto=" +  ;
          alup(idsuj2)
     = sqli(q1)
     q1 = "update saldosci set seller=" +  ;
          alup(idsuj1) +  ;
          " where  seller=" +  ;
          alup(idsuj2)
     = sqli(q1)
     q1 = "update saldosci set casher=" +  ;
          alup(idsuj1) +  ;
          " where  casher=" +  ;
          alup(idsuj2)
     = sqli(q1)
     q1 = "update detadocs set idsujeto=" +  ;
          alup(idsuj1) +  ;
          " where idsujeto=" +  ;
          alup(idsuj2)
     = sqli(q1)
     q1 = "update saldoscon set idaux=" +  ;
          alup(idsuj1) +  ;
          " where idaux=" +  ;
          alup(idsuj2)
     = sqli(q1)
     q1 = "update asientosmov set idaux=" +  ;
          alup(idsuj1) +  ;
          " where idaux=" +  ;
          alup(idsuj2)
     = sqli(q1)
     q1 = "update empleados set idsujeto=" +  ;
          alup(idsuj1) +  ;
          " where idsujeto=" +  ;
          alup(idsuj2)
     = sqli(q1)
     q1 = "update cheques set cliente=" +  ;
          alup(idsuj1) +  ;
          " where cliente=" +  ;
          alup(idsuj2)
     = sqli(q1)
     q1 = "update cheques set usercaja=" +  ;
          alup(idsuj1) +  ;
          " where usercaja=" +  ;
          alup(idsuj2)
     = sqli(q1)
     q1 = "update usuarios set idsujeto=" +  ;
          alup(idsuj1) +  ;
          " where idsujeto=" +  ;
          alup(idsuj2)
     = sqli(q1)
     q1 = "delete from empleados where idsujeto=" +  ;
          alup(idsuj2)
     = sqli(q1)
     q1 = "delete from usuarios where idsujeto=" +  ;
          alup(idsuj2)
     = sqli(q1)
     q1 = "delete from sujetos  where idsujeto=" +  ;
          alup(idsuj2)
     = sqli(q1)
     IF  .NOT. fintransq()
          = MESSAGEBOX( ;
            'No pudo ser actualizado ' +  ;
            ALLTRIM(ced_rep.sname),  ;
            0, empresa)
     ENDIF
     idsuj1 = 0
     idsuj2 = 0
     SELECT ced_rep
ENDSCAN
ENDPROC
**

q1 = " select code " +  ;
     "from vdocusmall " +  ;
     "where iddocu in (select distinct d.iddocu from gdoc g, dgdoc t, docuse d  " +  ;
     " where g.tag='ANXCOM' and g.idgdoc=t.idgdoc and t.dtag=d.dtag) "
IF sqli(q1, 'anxcom')
     SELECT anxcom
     GOTO TOP
     DO WHILE  .NOT. EOF()
          WAIT WINDOW NOWAIT  ;
               'Procesando ' +  ;
               ALLTRIM(STR(code))
          q1 = "select distinct case when rubname like '%CAJA%' 		then 1 else " +  ;
               "   case when rubname like '%NOTA%DEBITO%' 	then 6 else " +  ;
               "   case when rubname like '%DEPOSITO%' 	then 13 else " +  ;
               "   case when rubname like '%CUENTA%PAGAR%' then 2 else " +  ;
               "   case when rubname like '%CHEQUE%'		then 2 else " +  ;
               "   case when rubname like '%TARJETA%CREDITO%' then 10 end end end end end end as tipopag1 " +  ;
               "from vdocusmall d left join cobros c on (d.code=c.code) " +  ;
               "  left join rubros r on (c.rubcode=r.rubcode) " +  ;
               "where d.code=" +  ;
               alup(anxcom.code) +  ;
               " and not r.isdocret and " +  ;
               " d.iddocu in (select distinct d.iddocu from gdoc g, dgdoc t, docuse d  " +  ;
               " where g.tag='ANXCOM' and g.idgdoc=t.idgdoc and t.dtag=d.dtag) and " +  ;
               " ( rubname like '%CAJA%' or rubname like '%NOTA%DEBITO%' or rubname like '%CHEQUE%' or rubname like '%TARJETA%CREDITO%' ) " +  ;
               " union " +  ;
               "select distinct case when rubname like '%CAJA%' 		then 1 else " +  ;
               "  case when rubname like '%NOTA%DEBITO%' 	then 6 else " +  ;
               "  case when rubname like '%DEPOSITO%' 		then 13 else " +  ;
               "   case when rubname like '%CUENTA%PAGAR%' then 2 else " +  ;
               "  case when rubname like '%CHEQUE%'		then 2 else " +  ;
               "  case when rubname like '%TARJETA%CREDITO%' then 10 end end end end end end as tipopag1 " +  ;
               " from vdocusmall d left join cobros c on (d.code=c.code) " +  ;
               " left join rubros r on (c.rubcode=r.rubcode) " +  ;
               " left join detadocd t on (d.code=t.code) " +  ;
               " left join vdocusmall p on (t.coded=p.code) " +  ;
               "where not r.isdocret and p.code=" +  ;
               alup(anxcom.code) +  ;
               " and " +  ;
               " p.iddocu in (select distinct d.iddocu from gdoc g, dgdoc t, docuse d    " +  ;
               " where g.tag='ANXCOM' and g.idgdoc=t.idgdoc and t.dtag=d.dtag) and " +  ;
               " ( rubname like '%CAJA%' or rubname like '%NOTA%DEBITO%' or rubname like '%CHEQUE%' or rubname like '%TARJETA%CREDITO%' ) "
          IF sqli(q1, 'forpag')
               SELECT forpag
               DO CASE
                    CASE RECCOUNT('forpag') =  ;
                         0
                         q1 = "update aneiva set forpag1=1 where code=" +  ;
                              alup(anxcom.code)
                         = sqli(q1)
                    CASE RECCOUNT('forpag') =  ;
                         1
                         SELECT forpag
                         GOTO TOP
                         q1 = "update aneiva set forpag1=" +  ;
                              alup(forpag.tipopag1) +  ;
                              " where code=" +  ;
                              alup(anxcom.code)
                         = sqli(q1)
                    CASE RECCOUNT('forpag') =  ;
                         2
                         SELECT forpag
                         GOTO TOP
                         q1 = "update aneiva set forpag1=" +  ;
                              alup(forpag.tipopag1) +  ;
                              " where code=" +  ;
                              alup(anxcom.code)
                         = sqli(q1)
                         SKIP
                         q1 = "update aneiva set forpag2=" +  ;
                              alup(forpag.tipopag1) +  ;
                              " where code=" +  ;
                              alup(anxcom.code)
                         = sqli(q1)
                    CASE RECCOUNT('forpag') >  ;
                         2
                         SELECT forpag
                         GOTO TOP
                         q1 = "update aneiva set forpag1=" +  ;
                              alup(forpag.tipopag1) +  ;
                              " where code=" +  ;
                              alup(anxcom.code)
                         = sqli(q1)
                         SKIP
                         q1 = "update aneiva set forpag2=" +  ;
                              alup(forpag.tipopag1) +  ;
                              " where code=" +  ;
                              alup(anxcom.code)
                         = sqli(q1)
                         SKIP
                         q1 = "update aneiva set forpag3=" +  ;
                              alup(forpag.tipopag1) +  ;
                              " where code=" +  ;
                              alup(anxcom.code)
                         = sqli(q1)
               ENDCASE
          ENDIF
          SELECT anxcom
          SKIP
     ENDDO
ENDIF
ENDPROC
**

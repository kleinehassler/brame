PARAMETER fecdia
LOCAL fhoy
fhoy = fecdia
IF EMPTY(fhoy) .OR. ISNULL(fhoy)
     fhoy = hoy
ELSE
     IF fhoy = hoy
          RETURN
     ENDIF
ENDIF
q1 = "SELECT d.descripda AS variable, max(v.desde) as desde, v.hasta, v.valor, d.valorda AS tipod " +  ;
     "FROM vardiarias v LEFT JOIN detagru d ON (v.codvar = d.iddato) " +  ;
     "where " + alup(fhoy) +  ;
     ">=desde and " + alup(fhoy) +  ;
     "<=hasta " +  ;
     "group by variable, tipod, hasta, valor;"
IF  .NOT. sqli(q1, 'varhoy')
     = MESSAGEBOX( ;
       'No existe variables definidas, comuniquese con el centro de computo',  ;
       0, empresa)
     RETURN
ENDIF
SELECT varhoy
GOTO TOP
DO WHILE  .NOT. EOF()
     v = ALLTRIM(variable)
     t = ALLTRIM(tipod)
     DO CASE
          CASE t = 'N'
               vd = VAL(varhoy.valor)
          CASE t = 'C'
               vd = ALLTRIM(varhoy.valor)
          CASE tp = 'D'
               v = CTOD(ALLTRIM(varhoy.valor))
          CASE t = 'L'
               vd = IIF(ALLTRIM(varhoy.valor) =  ;
                    'S', '.t.',  ;
                    '.f.')
     ENDCASE
     public &v
     &v=vd
     SKIP
ENDDO
SELECT varhoy
USE
ENDPROC
**

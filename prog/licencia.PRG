frmprin = CREATEOBJECT("FORM")
frmprin.addobject("label1",  ;
                 "LABEL")
frmprin.addobject("LABEL2",  ;
                 "LABEL")
frmprin.addobject("TEXT1",  ;
                 "TEXTBOX")
frmprin.addobject("TEXT2",  ;
                 "TEXTBOX")
frmprin.addobject("COMMAND1",  ;
                 "CMDACEPTAR")
frmprin.addobject("COMMAND2",  ;
                 "CMDSALIR")
frmprin.visible = .T.
frmprin.enabled = .T.
frmprin.autocenter = .T.
frmprin.caption = 'Registrar fecha de Vencimiento'
frmprin.height = 140
frmprin.width = 300
WITH frmprin.label1
     .autosize = .T.
     .height = 17
     .left = 12
     .top = 25
     .width = 58
     .caption = 'Password'
     .fontbold = .T.
     .visible = .T.
ENDWITH
WITH frmprin.label2
     .autosize = .T.
     .height = 17
     .left = 12
     .top = 64
     .width = 124
     .caption = 'Fecha de Vencimiento'
     .fontbold = .T.
     .visible = .T.
ENDWITH
WITH frmprin.text1
     .height = 23
     .left = 152
     .top = 19
     .width = 100
     .inputmask = '!!!!!!!!!!!!!!'
     .passwordchar = '*'
     .visible = .T.
ENDWITH
WITH frmprin.text2
     .alignment = 3
     .height = 23
     .left = 152
     .top = 58
     .width = 100
     .value = {}
     .visible = .T.
ENDWITH
WITH frmprin.command1
     .caption = 'Aceptar'
     .height = 27
     .left = 48
     .top = 97
     .width = 84
     .fontbold = .T.
     .visible = .T.
ENDWITH
WITH frmprin.command2
     .caption = 'Salir'
     .height = 27
     .left = 148
     .top = 97
     .width = 84
     .fontbold = .T.
     .visible = .T.
ENDWITH
READ EVENTS
ENDPROC
**
DEFINE CLASS cmdaceptar AS  ;
       COMMANDBUTTON
**
PROCEDURE Click
     IF thisform.text1.value =  ;
        'DANIELPARDO'
          m.dato = conversion(thisform.text2.value)
          INSERT INTO licen FROM  ;
                 MEMVAR
          SELECT licen
          USE
          = MESSAGEBOX('Registrado',  ;
            0)
     ENDIF
     thisform.command2.click
ENDPROC
**
ENDDEFINE
**
DEFINE CLASS cmdsalir AS  ;
       COMMANDBUTTON
**
PROCEDURE Click
     CLEAR EVENTS
     thisform.release
ENDPROC
**
ENDDEFINE
**

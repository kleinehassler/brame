*------------------------------------------------------
* FUNCTION _StrTo128C(tcString) * CODIGO 128C
*------------------------------------------------------
* Convierte un string para ser impreso con
* fuente True Type "PF Barcode 128"
* Solo caracteres numéricos
* USO: _StrTo128C('1234567890')
* RETORNA: Caracter
*------------------------------------------------------
FUNCTION _StrTo128C(tcString)
  LOCAL lcStart, lcStop, lcRet, lcCheck, lcCar, ;
    lnLong, lnI, lnCheckSum, lnAsc
  lcStart = CHR(105 + 32)
  lcStop = CHR(106 + 32)
  lnCheckSum = ASC(lcStart) - 32
  lcRet = ALLTRIM(tcString)
  lnLong = LEN(lcRet)
  *--- La longitud debe ser par
  IF MOD(lnLong,2) # 0
    lcRet = '0' + lcRet
    lnLong = LEN(lcRet)
  ENDIF
  *--- Convierto los pares a caracteres
  lcCar = ''
  FOR lnI = 1 TO lnLong STEP 2
    lcCar = lcCar + CHR(VAL(SUBS(lcRet,lnI,2)) + 32)
  ENDFOR
  lcRet = lcCar
  lnLong = LEN(lcRet)
  FOR lnI = 1 TO lnLong
    lnAsc = ASC(SUBS(lcRet,lnI,1)) - 32
    lnCheckSum = lnCheckSum + (lnAsc * lnI)
  ENDFOR
  lcCheck = CHR(MOD(lnCheckSum,103) + 32)
  lcRet = lcStart + lcRet + lcCheck + lcStop
  *--- Esto es para cambiar los espacios y caracteres invalidos
  lcRet = STRTRAN(lcRet,CHR(32),CHR(232))
  lcRet = STRTRAN(lcRet,CHR(127),CHR(192))
  lcRet = STRTRAN(lcRet,CHR(128),CHR(193))
  RETURN lcRet
ENDFUNC

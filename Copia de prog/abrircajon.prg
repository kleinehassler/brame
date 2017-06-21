Set Device to Print 
Set Print On 
Set Console Off 

Corte de Papel
Chr(29) + Chr(86) + Chr(0)

Abre Cajon de Dinero
Chr(27) + Chr(112) + Chr(0) + Chr(25) + Chr(250)

****************
Private Sub Abre_cajon()
Open "LPT1" For Output As #1
Print #1, Chr$(27); "p"; Chr$(0); Chr$(25); Chr$(250)
Close #1
End Sub

****************

Cadena de texto + Chr(27) + "p" + Chr(0) + Chr(8) + Chr(32) + Chr(27) + Chr(105)

************

#include <fstream.h>
#include <stdlib.h>

int main()
{
char corte1=27;
char corte2='i';
ofstream impresora("PRN");
impresora<<endl<<corte1<<corte2;
impresora.close();
return 0;
}
***************************

1.- Chr(7)
2.-Chr(27)+Chr(112)+chr(0)+Chr(100)+Chr(250)
3.-= Chr(26)+Chr(27)+"J"+Chr(250)+chr(29)+"V"+chr(1)

**************************

Si la impresora es epson y de tiquets (Mod. TMU-XXX), puedes abrir el puerto como comentaron anteriormente, y con dirigir al mismo con el comando PRINT la secuencia que sigue:
chr(27)+"p"+chr(0)+chr(8)+chr(32), que es el codigo Esc de epson para apertura de cajón.
**************************

Para Cortar el Papel de manera Automatica del Impresor es CHR(27)&CHR(105)
Para abrir el Cajon del Dinero es CHR(27)&CHR(112)&CHR(0)

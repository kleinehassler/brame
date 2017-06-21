**** creación de acceso directo en el escritorio

WshShell = CreateObject("WScript.shell") 
strDesktop = wshShell.SpecialFolders("Desktop") 
oMyShortCut =WshShell.CreateShortcut(strDesktop+"\miejemplo.lnk") 
oMyShortCut.WindowStyle = 3 
*oMyShortCut.IconLocation = "c:\fox.ico" 
oMyShortCut.TargetPath = "%windir%\notepad.exe" 
oMyShortCut.HotKey = "ALT+CTRL+F" 
oMyShortCut.Save 
wait window 'Acceso directo creado' nowait 
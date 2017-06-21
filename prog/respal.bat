@echo off
set PGPASSWORD=mipassword 
pg_dump.exe -i -h 192.168.0.11 -p 5432 -U postgres -F c -b -v -f "respaldo.backup" mybase

set FECHA=%DATE% %TIME%
set FECHA=%FECHA:/=%
set FECHA=%FECHA::=-%
set FECHA=%FECHA:.=-%
ren respaldo.backup "%FECHA%".backup

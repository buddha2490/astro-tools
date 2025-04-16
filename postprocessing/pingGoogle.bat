@echo off
echo Starting continuous ping to google.com every 10 seconds...
:loop
powershell -Command "Write-Host 'Pinging google.com at ' (Get-Date); Test-Connection google.com -Count 1"
timeout /t 10 >nul
goto loop
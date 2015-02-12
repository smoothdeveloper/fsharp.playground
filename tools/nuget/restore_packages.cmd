NuGet.exe update -self
set packagedir=%cd%/packages
set nugetcmd=%cd%\NuGet.exe

forfiles /P ..\.. /S /M packages.config /C "cmd /c echo %nugetcmd% install @path -OutputDirectory %packagedir% -ExcludeVersion" > restoreall.cmd
call restoreall.cmd
del restoreall.cmd

pause
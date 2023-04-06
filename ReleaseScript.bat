IF EXIST .\TypeCobol.LanguageServer\bin\EI_Release (
IF EXIST .\CLI\src\bin\EI_Release (
IF EXIST .\EI_TypeCobol_Release (
RMDIR .\EI_TypeCobol_Release /S /Q
)
MKDIR .\EI_TypeCobol_Release
ECHO F | xcopy .\TypeCobol.LanguageServer\bin\EI_Release\*.exe .\EI_TypeCobol_Release /Y
ECHO F | xcopy .\TypeCobol.LanguageServer\bin\EI_Release\*.dll .\EI_TypeCobol_Release /Y
ECHO F | xcopy .\TypeCobol.LanguageServer\bin\EI_Release\*.runtimeconfig.json .\EI_TypeCobol_Release /Y
ECHO D | xcopy .\CLI\src\bin\EI_Release\DefaultCopies .\EI_TypeCobol_Release\DefaultCopies /E /Y
ECHO F | xcopy .\CLI\src\bin\EI_Release\*.exe .\EI_TypeCobol_Release /Y
ECHO F | xcopy .\CLI\src\bin\EI_Release\*.dll .\EI_TypeCobol_Release /Y
ECHO F | xcopy .\CLI\src\bin\EI_Release\*.runtimeconfig.json .\EI_TypeCobol_Release /Y
) ELSE (
ECHO "CLI EI_Release folder not found" )) ELSE (
ECHO "LanguageServer EI_Release folder not found" )


IF EXIST .\TypeCobol.LanguageServer\bin\Release (
IF EXIST .\CLI\src\bin\Release (
IF EXIST .\TypeCobol_Release (
RMDIR .\TypeCobol_Release /S /Q
)
MKDIR .\TypeCobol_Release
ECHO F | xcopy .\TypeCobol.LanguageServer\bin\Release\*.exe .\TypeCobol_Release /Y
ECHO F | xcopy .\TypeCobol.LanguageServer\bin\Release\*.dll .\TypeCobol_Release /Y
ECHO F | xcopy .\TypeCobol.LanguageServer\bin\Release\*.runtimeconfig.json .\TypeCobol_Release /Y
ECHO D | xcopy .\CLI\src\bin\Release\DefaultCopies .\TypeCobol_Release\DefaultCopies /E /Y
ECHO F | xcopy .\CLI\src\bin\Release\*.exe .\TypeCobol_Release /Y
ECHO F | xcopy .\CLI\src\bin\Release\*.dll .\TypeCobol_Release /Y
ECHO F | xcopy .\CLI\src\bin\Release\*.runtimeconfig.json .\TypeCobol_Release /Y
) ELSE (
ECHO "CLI Release folder not found" )) ELSE (
ECHO "LanguageServer Release folder not found" )

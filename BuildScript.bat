cd .\bin

IF EXIST .\EI_Release (
IF NOT EXIST .\EI_TypeCobol_Release (
ECHO D | xcopy .\EI_Release\Templates .\EI_TypeCobol_Release\Templates /E /Y
ECHO D | xcopy .\EI_Release\DefaultCopies .\EI_TypeCobol_Release\DefaultCopies /E /Y
ECHO F | xcopy .\EI_Release\config\skeletons.xml .\EI_TypeCobol_Release /E /Y
ECHO F | xcopy .\EI_Release\*.dll .\EI_TypeCobol_Release /Y
ECHO F | xcopy .\EI_Release\*.config .\EI_TypeCobol_Release /Y
ECHO F | xcopy .\EI_Release\*.exe .\EI_TypeCobol_Release /Y
DEL .\EI_TypeCobol_Release\*.Test.dll )) ELSE (
ECHO "EI_Release folder not found" )

IF EXIST .\Release (
IF NOT EXIST .\TypeCobol_Release (
ECHO D | xcopy .\Release\Templates .\TypeCobol_Release\Templates /E /Y
ECHO D | xcopy .\Release\DefaultCopies .\TypeCobol_Release\DefaultCopies /E /Y
ECHO F | xcopy .\Release\config\skeletons.xml .\TypeCobol_Release /E /Y
ECHO F | xcopy .\Release\*.dll .\TypeCobol_Release /Y
ECHO F | xcopy .\Release\*.config .\TypeCobol_Release /Y
ECHO F | xcopy .\Release\*.exe .\TypeCobol_Release /Y
DEL .\TypeCobol_Release\*.Test.dll )) ELSE (
ECHO "Release folder not found" )
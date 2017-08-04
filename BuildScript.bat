cd .\bin
IF EXIST .\EI_Release (
ECHO D | xcopy .\EI_Release\Templates .\ReleasePackage\Templates /E /Y
ECHO D | xcopy .\EI_Release\DefaultCopies .\ReleasePackage\DefaultCopies /E /Y
ECHO F | xcopy .\EI_Release\config\skeletons.xml .\ReleasePackage /E /Y
ECHO F | xcopy .\EI_Release\*.dll .\ReleasePackage /Y
ECHO F | xcopy .\EI_Release\*.config .\ReleasePackage /Y
ECHO F | xcopy .\EI_Release\*.exe .\ReleasePackage /Y
DEL .\ReleasePackage\*.Test.dll ) ELSE (
ECHO "EI_Release folder not found" )

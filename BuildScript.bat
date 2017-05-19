cd .\bin
IF EXIST .\EIRules_Release (
ECHO D | xcopy .\EIRules_Release\Templates .\ReleasePackage\Templates /E /Y
ECHO D | xcopy .\EIRules_Release\DefaultCopies .\ReleasePackage\DefaultCopies /E /Y
ECHO F | xcopy .\EIRules_Release\config\skeletons.xml .\ReleasePackage /E /Y
ECHO F | xcopy .\EIRules_Release\*.dll .\ReleasePackage /Y
ECHO F | xcopy .\EIRules_Release\*.config .\ReleasePackage /Y
ECHO F | xcopy .\EIRules_Release\*.exe .\ReleasePackage /Y
DEL .\ReleasePackage\*.Test.dll ) ELSE (
ECHO "EIRules_Release folder not found" )

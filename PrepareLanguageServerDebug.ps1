# Gather binaries required to launch a Debug instance of LanguageServer
# This script expects that all binaries for the given configuration are already fully built
$configuration = 'EI_Debug'
$output = ".\bin\LS\$configuration"

# Clean previous output
if (!(Test-Path -PathType Container $output))
{
    New-Item -ItemType Directory -Path $output
}
else
{
    Get-ChildItem $output -Recurse | Remove-Item -Recurse
}

# Copy binaries from LanguageServer
Copy-Item -Path ".\TypeCobol.LanguageServer\bin\$configuration\*.exe" -Destination $output
Copy-Item -Path ".\TypeCobol.LanguageServer\bin\$configuration\*.dll" -Destination $output
Copy-Item -Path ".\TypeCobol.LanguageServer\bin\$configuration\*.runtimeconfig.json" -Destination $output
Copy-Item -Path ".\TypeCobol.LanguageServer\bin\$configuration\*.pdb" -Destination $output

# Add binaries from CLI (for TypeCobol to Cobol 85 code generation)
Copy-Item -Path ".\CLI\src\bin\$configuration\*.exe" -Destination $output
Copy-Item -Path ".\CLI\src\bin\$configuration\*.dll" -Destination $output
Copy-Item -Path ".\CLI\src\bin\$configuration\*.runtimeconfig.json" -Destination $output
Copy-Item -Path ".\CLI\src\bin\$configuration\*.pdb" -Destination $output

# DefaultCopies folder
Copy-Item -Path ".\CLI\src\bin\$configuration\DefaultCopies" -Destination $output -Recurse

# Remove unwanted items
Remove-Item -Path "$output\CSCup.*"

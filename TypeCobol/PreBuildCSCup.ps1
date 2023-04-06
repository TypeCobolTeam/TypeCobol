# Current build configuration as first arg, output dir for generated classes as second arg
$configuration = $args[0]
$outputDir = $args[1]
Write-Host "Regenerating CUP classes into $outputDir for configuration $configuration..."

# Find CSCup.exe location
$projectDir = Get-Location
$CSCupPath = $projectDir | Split-Path | Join-Path -ChildPath 'CSCup'
$CSCupPath = $CSCupPath | Join-Path -ChildPath 'bin'
$CSCupPath = $CSCupPath | Join-Path -ChildPath "$configuration"
$CSCupPath = $CSCupPath | Join-Path -ChildPath 'CSCup.exe'

# Check CSCup.exe is available
if (!(Test-Path $CSCupPath))
{
    Write-Host "CSCup.exe not found ! Expected location is $CSCupPath."
    Exit 1
}

# Cleaning previous output
if (!(Test-Path -PathType Container $outputDir))
{
    New-Item -ItemType Directory -Path $outputDir
}
else
{
    Get-ChildItem $outputDir -Recurse | Remove-Item -Recurse
}

# Generate classes for Compiler Directive parser
$process = Start-Process -FilePath "$CSCupPath" -WorkingDirectory "$outputDir" -WindowStyle Hidden -ArgumentList "-nodate -nopositions -expect 2000 -parser CobolCompilerDirectivesParser -symbols CobolCompilerDirectivesSymbols $projectDir\Compiler\CupPreprocessor\CobolCompilerDirectives.cup" -Wait -PassThru
if ($process.ExitCode -ne 0) {
    Write-Host 'Could not generate classes for compiler directive parser !'
    Exit $process.ExitCode
}

# Generate classes for Program parser
$process = Start-Process -FilePath "$CSCupPath" -WorkingDirectory "$outputDir" -WindowStyle Hidden -ArgumentList "-nodate -nopositions -expect 2000 -parser TypeCobolProgramParser -symbols TypeCobolProgramSymbols $projectDir\Compiler\CupParser\TypeCobolProgram.cup" -Wait -PassThru
if ($process.ExitCode -ne 0) {
    Write-Host 'Could not generate classes for program parser !'
    Exit $process.ExitCode
}

Write-Host "All classes generated. PreBuildCSCup OK."

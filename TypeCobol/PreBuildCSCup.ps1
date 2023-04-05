# Current build configuration as first arg
$configuration = $args[0]

# Find CSCup.exe location
$projectDir = Get-Location
$outputDir = $projectDir | Split-Path | Join-Path -ChildPath 'CSCup'
$outputDir = $outputDir | Join-Path -ChildPath 'bin'
$outputDir = $outputDir | Join-Path -ChildPath "$configuration"
$CSCupPath = $outputDir | Join-Path -ChildPath 'CSCup.exe'

# Generate classes for Compiler Directive parser
$process = Start-Process -FilePath "$CSCupPath" -WorkingDirectory "$outputDir" -WindowStyle Hidden -ArgumentList "-nodate -nopositions -expect 2000 -parser CobolCompilerDirectivesParser -symbols CobolCompilerDirectivesSymbols $projectDir\Compiler\CupPreprocessor\CobolCompilerDirectives.cup" -Wait -PassThru
if ($process.ExitCode -ne 0) {
    Exit $process.ExitCode
}
Move-Item -Path "$outputDir\CobolCompilerDirectivesParser.cs" -Destination "$projectDir\Compiler\CupPreprocessor\" -Force
Move-Item -Path "$outputDir\CobolCompilerDirectivesSymbols.cs" -Destination "$projectDir\Compiler\CupPreprocessor\" -Force

# Generate classes for Program parser
$process = Start-Process -FilePath "$CSCupPath" -WorkingDirectory "$outputDir" -WindowStyle Hidden -ArgumentList "-nodate -nopositions -expect 2000 -parser TypeCobolProgramParser -symbols TypeCobolProgramSymbols $projectDir\Compiler\CupParser\TypeCobolProgram.cup" -Wait -PassThru
if ($process.ExitCode -ne 0) {
    Exit $process.ExitCode
}
Move-Item -Path "$outputDir\TypeCobolProgramParser.cs" -Destination "$projectDir\Compiler\CupParser\" -Force
Move-Item -Path "$outputDir\TypeCobolProgramSymbols.cs" -Destination "$projectDir\Compiler\CupParser\" -Force

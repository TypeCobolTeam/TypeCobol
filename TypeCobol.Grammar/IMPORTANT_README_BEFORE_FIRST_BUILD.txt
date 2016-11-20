IMPORTANT : perform this manual configuration step after nuget packages installation but before the first build to avoid compilation errors 
(due to a reference to the class TypeCobol.Compiler.AntlrUtils.LineAwareParser which is defined in another project)

1. Go the nuget Antlr package directory : TypeCobol\packages\Antlr4.4.5.3\build

2. Open the file "Antlr4.targets" in a text editor

3. At the bottom of the file, comment out the section that tries to compile the C# files generated for the parser in this project

<!--
    <ItemGroup>
      <Compile Include="@(Antlr4GeneratedCodeFiles)" />
      <_GeneratedCodeFiles Include="@(Antlr4GeneratedCodeFiles)" />
    </ItemGroup>
-->
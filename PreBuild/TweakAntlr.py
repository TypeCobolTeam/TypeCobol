#!/usr/bin/python

import sys
from xml.etree.ElementTree import parse, Element, register_namespace

def getattr(node, attr):
	try:
		return n.attrib[attr]
	except:
		return None


"""Document structure
<Project>
	...
	<Target Name="Antlr4CompileAddFilesGenerated"
			AfterTargets="Antlr4Compile"
			Condition="'@(Antlr4)' != ''">
		...
		<ItemGroup> <--- ItemGroup to delete
			<Compile Include="@(Antlr4GeneratedCodeFiles)" />
			<_GeneratedCodeFiles Include="@(Antlr4GeneratedCodeFiles)" />
		</ItemGroup>
	</Target>
</Project>
"""
def isToDelete(node):
	return len([n for n in node.getchildren() if n.tag.endswith('_GeneratedCodeFiles')]) > 0



if __name__ == '__main__':
	path = sys.argv[1]
	register_namespace('', 'http://schemas.microsoft.com/developer/msbuild/2003')
	dom = parse(path)
	root = dom.getroot() #Project
	target = [n for n in root.getchildren() if getattr(n, 'Name') == "Antlr4CompileAddFilesGenerated"][0]
	for itemgroup in target.getchildren():
		if isToDelete(itemgroup):
			target.remove(itemgroup)
	dom.write(path, xml_declaration=True)


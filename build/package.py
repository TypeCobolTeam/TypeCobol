#!/usr/bin/python
import os, zipfile, time

files = [
    'TypeCobol.CLI.exe',
    'SimpleMsgPack.dll',
    'Castle.Core.dll',
    'TypeCobol.Codegen.exe',
    'RazorEngine.dll',
    'TypeCobol.exe',
    'TypeCobol.Grammar.dll',
    'Antlr4.Runtime.dll',

    'System.Reactive.Core.dll',
    'System.Reactive.Interfaces.dll',
    'System.Reactive.Linq.dll',
    'System.Reactive.PlatformServices.dll',
    'System.Web.Razor.dll',

    os.path.join('config','skeletons.xml'),
    ]

if __name__ == '__main__':
    current_directory = os.path.dirname(os.path.realpath(__file__))
    path = os.path.join(current_directory, os.pardir, 'bin', 'Debug')
    archive_name = 'TypeCobol.CLI-%s.zip' % time.strftime('%Y%m%d-%H%M%S')
    archive = zipfile.ZipFile(archive_name, 'w')
    for f in files:
        archive.write(os.path.join(path, f), os.path.basename(f))
    readme = 'README.txt'
    os.system('%s --help > %s' % (os.path.join(path, files[0]), readme))
    archive.write(readme, readme)
    os.remove(readme)
    archive.close()


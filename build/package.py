#!/usr/bin/python
import os, zipfile, time

files = [
    # assemblies (@see ildasm)
    'TypeCobol.CLI.exe',
      'TypeCobol.Codegen.exe',
        'RazorEngine.dll',
          'System.Web.Razor.dll',
      'SimpleMsgPack.dll',
      'TypeCobol.exe',
        'Antlr4.Runtime.dll',
      'TypeCobol.Transform.exe',
    # resource files
    os.path.join('config','skeletons.xml'),
    ]

if __name__ == '__main__':
    current_directory = os.path.dirname(os.path.realpath(__file__))
    root = os.path.join(current_directory, os.pardir)
    path = os.path.join(root, 'bin', 'Debug')
    now = time.strftime('%Y%m%d-%H%M%S')
    archive_name = 'TypeCobol.CLI-%s.zip' % now
    archive = zipfile.ZipFile(archive_name, 'w')
    for f in files:
        archive.write(os.path.join(path, f), os.path.basename(f))
    readme = 'README.txt'
    os.system('echo -ne "TAG:\\n  " > %s' % readme)
    res = os.system('git --git-dir=%s/.git rev-parse HEAD >> %s' % (root, readme))
    if (res > 0):
        os.system('echo "%s self-delivery on %s." >> %s' % (os.getenv('USERNAME'), now, readme))
    os.system('echo >> %s' % readme)
    os.system('%s --help >> %s' % (os.path.join(path, files[0]), readme))
    archive.write(readme, readme)
    os.remove(readme)
    archive.close()


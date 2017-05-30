#!/usr/bin/python
import os, zipfile, time, sys

"""
Usage
py package.py
	will package 4 build configurations: Debug, Release, EIRules_Debug, EIRules_Release
	
py package.py CONFIG_NAME
	will build only the named configuration
	CONFIG_NAME must be a valid configuration: Debug, Release, EIRules_Debug, EIRules_Release


"""
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
      'TypeCobol.Analytics.dll',
        'Microsoft.ApplicationInsights.dll',
	'NLog.dll',
    # resource files
    os.path.join('config','skeletons.xml'),
    ]

#From http://stackoverflow.com/questions/1855095/how-to-create-a-zip-archive-of-a-directory/
def addDirToZip(zipHandle, path, basePath=""):
    """
    Adding directory given by \a path to opened zip file \a zipHandle

    @param basePath path that will be removed from \a path when adding to archive

    Examples:
        # add whole "dir" to "test.zip" (when you open "test.zip" you will see only "dir")
        zipHandle = zipfile.ZipFile('test.zip', 'w')
        addDirToZip(zipHandle, 'dir')
        zipHandle.close()

        # add contents of "dir" to "test.zip" (when you open "test.zip" you will see only it's contents)
        zipHandle = zipfile.ZipFile('test.zip', 'w')
        addDirToZip(zipHandle, 'dir', 'dir')
        zipHandle.close()

        # add contents of "dir/subdir" to "test.zip" (when you open "test.zip" you will see only contents of "subdir")
        zipHandle = zipfile.ZipFile('test.zip', 'w')
        addDirToZip(zipHandle, 'dir/subdir', 'dir/subdir')
        zipHandle.close()

        # add whole "dir/subdir" to "test.zip" (when you open "test.zip" you will see only "subdir")
        zipHandle = zipfile.ZipFile('test.zip', 'w')
        addDirToZip(zipHandle, 'dir/subdir', 'dir')
        zipHandle.close()

        # add whole "dir/subdir" with full path to "test.zip" (when you open "test.zip" you will see only "dir" and inside it only "subdir")
        zipHandle = zipfile.ZipFile('test.zip', 'w')
        addDirToZip(zipHandle, 'dir/subdir')
        zipHandle.close()

        # add whole "dir" and "otherDir" (with full path) to "test.zip" (when you open "test.zip" you will see only "dir" and "otherDir")
        zipHandle = zipfile.ZipFile('test.zip', 'w')
        addDirToZip(zipHandle, 'dir')
        addDirToZip(zipHandle, 'otherDir')
        zipHandle.close()
    """
    basePath = basePath.rstrip("\\/") + ""
    basePath = basePath.rstrip("\\/")
    for root, dirs, files in os.walk(path):
        # add dir itself (needed for empty dirs
        #zipHandle.write(os.path.join(root, "."))
        # add files
        for file in files:
            filePath = os.path.join(root, file)
            inZipPath = filePath.replace(basePath, "", 1).lstrip("\\/")
            #print filePath + " , " + inZipPath
            zipHandle.write(filePath, inZipPath)

def packageTypeCobol(buildConfig):
    current_directory = os.path.dirname(os.path.realpath(__file__))
    root = os.path.join(current_directory, os.pardir)
    path = os.path.join(root, 'bin', buildConfig)
    now = time.strftime('%Y%m%d-%H%M%S')
    archive_name = 'TypeCobol.CLI-' + buildConfig + '-%s.zip' % now
    archive = zipfile.ZipFile(archive_name, 'w')
    for f in files:
        archive.write(os.path.join(path, f), os.path.basename(f))
    addDirToZip(archive, os.path.join(path, 'DefaultCopies'), path)

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

if __name__ == '__main__':
    if len(sys.argv) > 1:
        packageTypeCobol(sys.argv[1])
    else:
        packageTypeCobol('Debug')
        packageTypeCobol('Release')
        packageTypeCobol('EIRules_Debug')
        packageTypeCobol('EIRules_Release')

namespace TypeCobol.LanguageServer.Test.LanguageServerRobot.Installer
{
    internal static class WhyThisProject
    {
        /*
         * This project allows to download the TypeCobol.LanguageServerRobot executable
         * as a NuGet package and to have all of its required files to run located
         * in an isolated directory, which is the output directory of this project.
         * 
         * While LSR is used by LanguageServer unit tests project, it cannot reside
         * in the output directory of the test project itself because it interferes with
         * the test discovery process. MSTest framework seems unable to deal with both
         * .exe file and .dll file having the same name... Having both TypeCobol.LanguageServerRobot.exe
         * and TypeCobol.LanguageServerRobot.dll in the output dir of TypeCobol.LanguageServer.Test
         * will break the test discovery process.
         * 
         * Note that both files are required: the dll is the portable part of the app
         * and the exe is platform-specific. The runtimeconfig.json is also required to
         * run the executable so it is copied from NuGet package as well.
         */
    }
}

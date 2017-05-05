using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.DirectoryServices.AccountManagement;
using System.Net.Mail;
using System.Configuration;
using System.IO;
using System.Diagnostics;

namespace TypeCobol
{
    public static class MailSender
    {
        private static string _CurrentUserMail = UserPrincipal.Current.EmailAddress; //This could be very slow out of a domain or even crash (but we are in a domain)

        /// <summary>
        /// Mail sender method. This method will send an error email with the different configurations given in App.config file. 
        /// This method only send emails while in RELEASE.
        /// </summary>
        /// <param name="exception"></param>
        /// <param name="sourceFilePaths"></param>
        /// <param name="CopyFolders"></param>
        /// <param name="config"></param>
        public static void Send(Exception exception, List<string> sourceFilePaths, List<string> CopyFolders, string config)
        {
#if !DEBUG 
            var mail = new MailMessage();
            var smtpClient = new SmtpClient();
            var errorPagePath = Path.GetDirectoryName(Process.GetCurrentProcess().MainModule.FileName) + @"\config\Error.html";
            string errorTemplate = "Oh.. template not found..";

            smtpClient.Host = ConfigurationManager.AppSettings["SmtpServer"];
            smtpClient.Port = int.Parse(ConfigurationManager.AppSettings["SmtpPort"]);
            smtpClient.UseDefaultCredentials = bool.Parse(ConfigurationManager.AppSettings["SmtpUseDefaultCredential"]);
            smtpClient.DeliveryMethod = SmtpDeliveryMethod.Network;

            var receiver = ConfigurationManager.AppSettings["MailReceiver"];
            mail.To.Add(new MailAddress(receiver));
            mail.From = new MailAddress(_CurrentUserMail);
            mail.Subject = ConfigurationManager.AppSettings["MailSubject"];

            if(File.Exists(errorPagePath))
            {
                errorTemplate = File.ReadAllText(errorPagePath);
                errorTemplate = errorTemplate.Replace("{DateTime}", DateTime.Now.ToString());

                errorTemplate = errorTemplate.Replace("{User}", _CurrentUserMail.Split('@')[0].Replace('.', ' '));
                errorTemplate = errorTemplate.Replace("{TypeCobolVersion}", ConfigurationManager.AppSettings["TypeCobolVersion"]);
                errorTemplate = errorTemplate.Replace("{Source}", exception.Source);

                errorTemplate = errorTemplate.Replace("{Config}", config);
                errorTemplate = errorTemplate.Replace("{Message}", exception.Message);
                errorTemplate = errorTemplate.Replace("{StackTrace}", exception.StackTrace);

                string copiesDump = null;
     
                if(CopyFolders != null)
                    foreach (var folder in CopyFolders)
                    {
                        if (Directory.Exists(folder))
                            foreach (var copy in Directory.EnumerateFiles(folder, "*", SearchOption.AllDirectories))
                            {
                                copiesDump += copy.Split('\\').Last() + " ";
                                if (File.Exists(copy))
                                    copiesDump += File.GetCreationTime(copy) + "\n";
                                else
                                    copiesDump += "Not found. \n";
                            }
                    }
                
                errorTemplate = errorTemplate.Replace("{CopiesDump}", copiesDump == null ? "No copy found" : copiesDump);
            }

            mail.IsBodyHtml = true;
            mail.Body = errorTemplate;

            if(sourceFilePaths != null)
                foreach (var sourceFilePath in sourceFilePaths)
                {
                    if (!string.IsNullOrEmpty(sourceFilePath) && File.Exists(sourceFilePath))
                        mail.Attachments.Add(new Attachment(sourceFilePath));
                }
          

            smtpClient.Send(mail);
#endif
        }

    }
}

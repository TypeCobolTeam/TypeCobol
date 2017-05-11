using System;
using System.Collections.Generic;
using System.Configuration;
using System.Linq;
using System.Threading.Tasks;
using System.Net.Mail;
using System.IO;
using System.Diagnostics;
using System.DirectoryServices.AccountManagement;
using System.Reflection;
using Microsoft.ApplicationInsights;
using Microsoft.ApplicationInsights.Extensibility;

namespace Analytics
{
    /// <summary>
    /// Analytics Wrapper is Singleton designed class. Please use AnalyticsWrapper.Instance to work with it. 
    /// </summary>
    public sealed class AnalyticsWrapper
    {
        private static readonly Lazy<AnalyticsWrapper> _LazyAccess = new Lazy<AnalyticsWrapper>(() => new AnalyticsWrapper()); //Singleton pattern
        private static bool _DisableTelemetry = true; //By default telemetry needs to be disable. It will only be enable by the first caller.
        private static TelemetryClient _TelemetryClient;
        private Configuration _AppConfig; 
        private AnalyticsWrapper()
        {
            _AppConfig = ConfigurationManager.OpenExeConfiguration(Assembly.GetExecutingAssembly().Location); //Load custom app.config for this assembly
            var appKey = _AppConfig.AppSettings.Settings["AppInsightKey"].Value;//Get API Key CLI project config file

            // ----- Initiliaze AppInsights Telemetry Client -------//
            _TelemetryClient = new TelemetryClient(new TelemetryConfiguration(appKey));
            _TelemetryClient.Context.User.Id = Environment.UserName;
            _TelemetryClient.Context.Session.Id = Guid.NewGuid().ToString();
            _TelemetryClient.Context.Device.OperatingSystem = Environment.OSVersion.ToString();
            // --------------------------------------- //
        }

        /// <summary>
        /// Instance to use to track telemetry of the application
        /// </summary>
        public static AnalyticsWrapper Telemetry { get { return _LazyAccess.Value; } }//Singleton pattern

        /// <summary>
        /// Gets or sets a value indicating whether sending of telemetry is disabled. 
        /// </summary>
        public bool DisableTelemetry { get { return _DisableTelemetry; } set { _DisableTelemetry = value; } }

        /// <summary>
        /// Track a new event into analytics collector
        /// </summary>
        /// <param name="eventName">Event name to store</param>
        public void TrackEvent(string eventName)
        {
            if (_DisableTelemetry) return;
            _TelemetryClient.TrackEvent(eventName);
        }

        /// <summary>
        /// Track a new exception into analytics collector
        /// </summary>
        /// <param name="exception">Exception raised to store</param>
        public void TrackException(Exception exception)
        {
            if (_DisableTelemetry) return;
            _TelemetryClient.TrackException(exception);
        }

        /// <summary>
        /// Method to end the current Telemetry session. It will also force data sending to Application Inshights.
        /// </summary>
        public void EndSession()
        {
            _TelemetryClient.Flush();
        }


        public void SendMail(Exception exception, List<string> sourceFilePaths, List<string> CopyFolders, string config)
        {
            if (_DisableTelemetry) return;

            var currentUserMail = UserPrincipal.Current.EmailAddress;
            var mail = new MailMessage();
            var smtpClient = new SmtpClient();
            var errorPagePath = Path.GetDirectoryName(Process.GetCurrentProcess().MainModule.FileName) + @"\Templates\ErrorMailTemplate.html";
            string errorTemplate = "<h3>Oh.. template not found..</h3>";

            smtpClient.Host = _AppConfig.AppSettings.Settings["SmtpServer"].Value;
            smtpClient.Port = int.Parse(_AppConfig.AppSettings.Settings["SmtpPort"].Value);
            smtpClient.UseDefaultCredentials = bool.Parse(_AppConfig.AppSettings.Settings["SmtpUseDefaultCredential"].Value);
            smtpClient.DeliveryMethod = SmtpDeliveryMethod.Network;

            var receiver = _AppConfig.AppSettings.Settings["MailReceiver"].Value;
            mail.To.Add(new MailAddress(receiver));
            mail.From = new MailAddress(currentUserMail);
            mail.Subject = _AppConfig.AppSettings.Settings["MailSubject"].Value;

            if(File.Exists(errorPagePath))
            {
                errorTemplate = File.ReadAllText(errorPagePath);
                errorTemplate = errorTemplate.Replace("{DateTime}", DateTime.Now.ToString());

                errorTemplate = errorTemplate.Replace("{User}", currentUserMail.Split('@')[0].Replace('.', ' '));
                errorTemplate = errorTemplate.Replace("{TypeCobolVersion}", _AppConfig.AppSettings.Settings["TypeCobolVersion"].Value);
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

#if !DEBUG
            smtpClient.Send(mail);
#endif
        }
    }
}

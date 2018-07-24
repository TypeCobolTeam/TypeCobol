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
using NLog;
using System.Text;
using System.Security.Cryptography;

namespace Analytics
{
    /// <summary>
    /// Use this to determine which type of event you want to send. 
    /// If analyticswrapper state is CodeGeneration, every event will be send. 
    /// Ifr Completion stat is active, only completion envent will be send. 
    /// </summary>
    public enum TelemetryVerboseLevel
    {
        Disable = 0,
        Completion = 1,
        CodeGeneration = 2,
    }

    /// <summary>
    /// Type of Analytics event. This will allow AnalyticsWrapper 
    /// if the event has to send or not regarding to TelemetryVerboseLevel. 
    /// </summary>
    public enum LogType
    {
        Completion = 0,
        TypeCobolUsage = 1,
        Genration = 2
    }

    public enum EventType
    {
        Duration, 
        Diagnostic,
        ReturnCode,
        Generation,
        Hover,
        Completion,
        Definition,
        SignatureHelp,
        TypeDeclared,
        FunctionDeclared,
        TypedUsed,
        FunctionCalled,
        Exception,
    }


    /// <summary>
    /// Analytics Wrapper is Singleton designed class. Please use AnalyticsWrapper.Instance to work with it. 
    /// </summary>
    public sealed class AnalyticsWrapper
    {
        private static readonly Lazy<AnalyticsWrapper> _LazyAccess = new Lazy<AnalyticsWrapper>(() => new AnalyticsWrapper()); //Singleton pattern
       
        
        private Configuration _AppConfig;
        private string _TypeCobolVersion;
        private static Logger logger = LogManager.GetCurrentClassLogger();

        private AnalyticsWrapper()
        {
            try
            {
                _AppConfig = ConfigurationManager.OpenExeConfiguration(Assembly.GetExecutingAssembly().Location); //Load custom app.config for this assembly
                _TypeCobolVersion = _AppConfig.AppSettings.Settings["TypeCobolVersion"].Value;

                //Init base event, contains version, generated session id, user id.
                _EventBase = new TelemetryEventBase()
                {
                    SessionId = Guid.NewGuid().ToString(),
                    UserId = Environment.UserName,
                    TypeCobolVersion = _TypeCobolVersion
                };
            }
            catch (Exception e) { logger.Fatal(e); }
        }

        private TelemetryEventBase _EventBase;

        /// <summary>
        /// Instance to use to track telemetry of the application
        /// </summary>
        public static AnalyticsWrapper Telemetry { get { return _LazyAccess.Value; } }//Singleton pattern

        /// <summary>
        /// Gets or sets a value indicating whether sending of telemetry is disabled. 
        /// </summary>
        public TelemetryVerboseLevel TelemetryVerboseLevel { get; set; } = TelemetryVerboseLevel.Disable;


        public string TypeCobolVersion { get { return _TypeCobolVersion; } }

        /// <summary>
        /// Track a new event into analytics collector
        /// </summary>
        /// <param name="eventName">Text name of the event</param>
        /// <param name="properties">Named string values you can use to search and classify events.</param>
        /// <param name="metrics">Measurements associated with this event.</param>
        public void TrackEvent(EventType eventType, string content, LogType logType)
        {
            try
            {
                if (TelemetryVerboseLevel == TelemetryVerboseLevel.Disable) return;
                
                if ((TelemetryVerboseLevel == TelemetryVerboseLevel.Completion && logType == LogType.Completion)
                    || (TelemetryVerboseLevel > TelemetryVerboseLevel.Completion && logType > LogType.Completion))
                {
                    ElasticTelemetry.Elastic.IndexEvent( new TelemetryEvent(_EventBase, eventType, content));
                }

            }
            catch (Exception e) { logger.Fatal(e); }
        }

        /// <summary>
        /// Track a new event with metrics data
        /// </summary>
        /// <param name="evenType"></param>
        /// <param name="logType"></param>
        /// <param name="metricName"></param>
        /// <param name="metricValue"></param>
        public void TrackMetricsEvent(EventType eventType, LogType logType, string metricName, double metricValue)
        {
            try
            {
                if (TelemetryVerboseLevel == TelemetryVerboseLevel.Disable) return;
                
                if ((TelemetryVerboseLevel == TelemetryVerboseLevel.Completion && logType == LogType.Completion)
                    || (TelemetryVerboseLevel > TelemetryVerboseLevel.Completion && logType > LogType.Completion))
                {
                    ElasticTelemetry.Elastic.IndexEvent( new TelemetryMetricsEvent(_EventBase, eventType, metricName, metricValue));
                }

            }
            catch (Exception e) { logger.Fatal(e); }
        }

        /// <summary>
        /// Track a new exception into analytics collector
        /// </summary>
        /// <param name="exception">Exception raised to store</param>
        public void TrackException(Exception exception)
        { 
            try
            {
                if (TelemetryVerboseLevel == TelemetryVerboseLevel.Disable) return;
                ElasticTelemetry.Elastic.IndexEvent( new TelemetryExceptionEvent(_EventBase, exception));
            }
            catch (Exception e) { logger.Fatal(e); }
        }

        public void SendMail(Exception exception, List<string> sourceFilePaths, List<string> CopyFolders, string config)
        {
            try
            {
                if (TelemetryVerboseLevel == TelemetryVerboseLevel.Disable) return;

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

                if (File.Exists(errorPagePath))
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

                    if (CopyFolders != null)
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

                if (sourceFilePaths != null)
                    foreach (var sourceFilePath in sourceFilePaths)
                    {
                        if (!string.IsNullOrEmpty(sourceFilePath) && File.Exists(sourceFilePath))
                            mail.Attachments.Add(new Attachment(sourceFilePath));
                    }

#if !DEBUG
            smtpClient.Send(mail);
#endif
            }
            catch (Exception e) { logger.Fatal(e); }
        }

        private static string CreateSHA256(string text)
        {
            byte[] input = Encoding.UTF8.GetBytes(text.TrimEnd('\0'));
            byte[] hash = new SHA256Managed().ComputeHash(input);
            var result = new StringBuilder();
            foreach (byte b in hash)
                result.Append(System.String.Format("{0:x2}", b));
            return result.ToString();
        }
    }
}

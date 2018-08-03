using System;
using System.Collections.Generic;
using System.Configuration;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;
using Elasticsearch.Net;
using NLog;

namespace Analytics
{
    internal sealed class ElasticTelemetry
    {
        private static readonly Lazy<ElasticTelemetry> _LazyAccess = new Lazy<ElasticTelemetry>(() => new ElasticTelemetry()); //Singleton pattern
        /// <summary>
        /// Instance to use to track telemetry of the application
        /// </summary>
        internal static ElasticTelemetry Elastic { get { return _LazyAccess.Value; } }//Singleton pattern

        private ElasticTelemetry()
        {
            try
            {
                //Load custom app.config for this assembly
                var appConfig = ConfigurationManager.OpenExeConfiguration(Assembly.GetExecutingAssembly().Location); 

                //Configure connection settings 
                var connectionSettings = new ConnectionConfiguration(new Uri(appConfig.AppSettings.Settings["ElasticServer"].Value));
                connectionSettings
                    .BasicAuthentication(appConfig.AppSettings.Settings["ElasticUser"].Value, appConfig.AppSettings.Settings["ElasticPassword"].Value)
                    .RequestTimeout(TimeSpan.FromSeconds(30))
                    .DisableDirectStreaming();

                //Create Elastic client
                _ElasticClient = new ElasticLowLevelClient(connectionSettings);

                //Set Elastic Index and Type
                _ElasticIndex = appConfig.AppSettings.Settings["ElasticIndex"].Value;
                _ElasticType = appConfig.AppSettings.Settings["ElasticType"].Value;
            }
            catch (Exception e) { logger.Fatal(e); }
        }

        private ElasticLowLevelClient _ElasticClient;
        private string _ElasticIndex;
        private string _ElasticType;
        private static Logger logger = LogManager.GetCurrentClassLogger();

        internal void IndexEvent(TelemetryEventBase plainEvent)
        {
            try
            {
                _ElasticClient.Index<BytesResponse>(_ElasticIndex, _ElasticType, PostData.Serializable(plainEvent));
            }
            catch (Exception e) { logger.Fatal(e); }
        }
    }
}

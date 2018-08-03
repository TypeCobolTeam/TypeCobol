using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Analytics;

namespace Analytics
{
    internal class TelemetryEventBase
    {
        public TelemetryEventBase() { }
        public TelemetryEventBase(string sessionId, string userId, string typeCobolVersion)
        {
            SessionId = sessionId;
            UserId = userId;
            TypeCobolVersion = typeCobolVersion;
            Date = DateTime.Now;
        }

        public string TypeCobolVersion { get; set; }
        public string UserId { get; set; }
        public string SessionId { get; set; }
        public DateTime Date { get; set; }
        public EventType EventType { get; set; }
        public string EvenTypeString
        {
            get { return EventType.ToString(); }
        }
    }

    internal class TelemetryEvent : TelemetryEventBase
    {
        public TelemetryEvent(TelemetryEventBase baseEvent, EventType evenType, string content) : base(baseEvent.SessionId, baseEvent.UserId, baseEvent.TypeCobolVersion)
        {
            EventType = evenType;
            Content = content;
        }

        public string Content { get; set; }

    }

    internal class TelemetryMetricsEvent : TelemetryEventBase
    {
        public TelemetryMetricsEvent(TelemetryEventBase baseEvent, EventType evenType, string metricname, double metricValue) : base(baseEvent.SessionId, baseEvent.UserId, baseEvent.TypeCobolVersion)
        {
            EventType = evenType;
            MetricName = metricname;
            MetricValue = metricValue;
        }

        public string MetricName { get; set; }
        public double MetricValue { get; set; }
    }

    internal class TelemetryExceptionEvent : TelemetryEventBase
    {
        public TelemetryExceptionEvent(TelemetryEventBase baseEvent, Exception exception) : base(baseEvent.SessionId, baseEvent.UserId, baseEvent.TypeCobolVersion)
        {
            Exception = exception;
            EventType = EventType.Exception;
        }
        public Exception Exception { get; set; }
    }
}

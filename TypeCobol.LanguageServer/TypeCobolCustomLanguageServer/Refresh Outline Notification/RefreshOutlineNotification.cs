using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.LanguageServer.JsonRPC;


namespace TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol
{ 
    class RefreshOutlineNotification
    {
        public static readonly NotificationType Type = new NotificationType("typecobol/refreshOutline", typeof(RefreshOutlineParams));
    }
}

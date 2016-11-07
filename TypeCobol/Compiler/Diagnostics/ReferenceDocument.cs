using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.Compiler.Diagnostics
{
    public class ReferenceDocument
    {
        public ReferenceDocument(int id, string name, string version, string downloadLink, string downloadDate)
        {
            Id = id;
            Name = name;
            Version = version;
            DownloadLink = downloadLink;
            DownloadDate = downloadDate;
        }

        public int Id { get; private set; }
        public string Name { get; private set; }
        public string Version { get; private set; }
        public string DownloadLink { get; private set; }
        public string DownloadDate { get; private set; }

        // All reference documents quoted in diagnostic messages

        public static readonly ReferenceDocument[] GetFromCode = new ReferenceDocument[] {
            null, // to start document codes at index 1
            new ReferenceDocument(1, "IBM Enterprise Cobol for zOS - Language Reference", "5.1.1", "http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr11.pdf", "2014-08-15"),
            new ReferenceDocument(2, "IBM Enterprise Cobol for zOS - Programming Guide", "5.1.1", "http://publibfp.boulder.ibm.com/epubs/pdf/igy5pg11.pdf", "2014-08-15"),
            new ReferenceDocument(3, "DB2 11 for zOs - Application Programming and SQL Guide", "DB2 11 Fourth edition (June 2014)", "http://publib.boulder.ibm.com/epubs/pdf/dsnapn03.pdf", "2014-08-15"),
            new ReferenceDocument(4, "ISO/IEC 1989:2014 : Information technology - Programming languages, their environments and system software interfaces -- Programming language COBOLs", "Second edition 2014-06-01", "http://www.iso.org/iso/home/store/catalogue_tc/catalogue_detail.htm?csnumber=51416", "2016"),
            new ReferenceDocument(5, "Source code", System.Reflection.Assembly.GetExecutingAssembly().GetName().Version.ToString(), "https://github.com/TypeCobolTeam/TypeCobol", DateTime.Now.Year.ToString()),
        };
    }
}

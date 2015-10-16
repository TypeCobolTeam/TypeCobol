using System.Collections.Generic;
using System.IO;
using System.Text;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Test.Compiler.Parser
{
    class TestJSONSerializer
    {
        private void kv(StringBuilder b, string key, string value)
        {
            b.Append("\"").Append(key).Append("\": ").Append(value);
        }
        private void newline(StringBuilder b, int a, string comma)
        {
            b.Append(comma).Append("\n").Append(i(a + 1));
        }

        private string ToJSON(int a, Token t)
        {
            var b = new StringBuilder("{");
            newline(b, a+1, "");
            kv(b, "Type", "\"" + t.TokenType + "\"");
            newline(b, a+1, ",");
            kv(b, "Text", "\"" + escape(t.Text) + "\"");
            newline(b, a+1, ",");
            kv(b, "Line", ""+t.Line);
            newline(b, a+1, ",");
            kv(b, "Column", ""+t.Column);
            newline(b, a+1, ",");
            kv(b, "EndColumn", ""+t.EndColumn);
            newline(b, a+1, ",");
            kv(b, "UsesVirtualSpaceAtEndOfLine", ""+t.UsesVirtualSpaceAtEndOfLine.ToString().ToLower());
            newline(b, a, "");
            return b.Append("}").ToString();
        }

        private string escape(string text)
        {
            text = text.Replace("\"", "\\\"");
            text = text.Replace("\'", "\\\'");
            return text;
        }

        private string ToJSON(int a, Diagnostic d)
        {
            var b = new StringBuilder("{");
            newline(b, a+1, "");
            kv(b, "ColumnStart", ""+d.ColumnStart);
            newline(b, a+1, ",");
            kv(b, "ColumnEnd", ""+d.ColumnEnd);
            newline(b, a+1, ",");
            kv(b, "Message", ""+"\""+d.Message+"\"");
            newline(b, a+1, ",");
            kv(b, "Code", ""+"\""+d.Info.Code+"\"");
            newline(b, a+1, ",");
            kv(b, "Severity", ""+"\""+d.Info.Severity+"\"");
            newline(b, a+1, ",");
            kv(b, "Category", ""+"\""+d.Info.Category+"\"");
            newline(b, a, "");
            return b.Append("}").ToString();
        }

        internal string ToJSON(int a, TypeCobol.Compiler.CodeElements.CodeElement e)
        {
            var b = new StringBuilder("\n").Append(i(a)).Append("{");
            newline(b, a+1, "");
            kv(b, "Type", "\"" + e.Type + "\"");
            newline(b, a+1, ",");
            kv(b, "FirstTokenLineIndexInMainDocument", "" + e.FirstTokenLineIndexInMainDocument);
            newline(b, a+1, ",");
            kv(b, "LastTokenLineIndexInMainDocument", "" + e.LastTokenLineIndexInMainDocument);
            newline(b, a+1, ",");
            b.Append("\"ConsumedTokens\": [");
            bool first = true;
            foreach (var t in e.ConsumedTokens)
            {
                if (first) {
                    newline(b, a+3, "");
                    first = false;
                } else newline(b, a+3, ",");
                b.Append(ToJSON(a+3, t));
            }
            newline(b, a+1, "");
            b.Append("]");

            newline(b, a+1, ",");

            b.Append("\"Diagnostics\": [");
            first = true;
            foreach (var d in e.Diagnostics)
            {
                if (first) {
                    newline(b, a+3, "");
                    first = false;
                } else newline(b, a+3, ",");
                b.Append(ToJSON(a+3, d));
            }
            newline(b, a + 1, "");
            b.Append("]");
            newline(b, a, "");
            return b.Append("}").ToString();
        }

        internal string ToJSON(IList<CodeElement> elements)
        {
            if (elements == null) return "";
            int a = 3;
            var buffer = new StringBuilder("{\n").Append(i(a)).Append("\"CodeElements\": [");
            foreach (var element in elements)
            {
                buffer.Append(ToJSON(a+1, element)).Append(",");
            }
            return buffer.Append("\n").Append(i(a)).Append("]\n}").ToString();
        }

        private string i(int alinea)
        {
            var buffer = new StringBuilder("");
            string unit = "  ";
            for (int i = 0; i < alinea; i++) buffer.Append(unit);
            return buffer.ToString();
        }

        public static void DumpAsJSON(IList<CodeElement> elements, string filename)
        {
            string json = new TestJSONSerializer().ToJSON(elements);
            string[] lines = { json };
            System.IO.File.WriteAllLines(filename + ".json", lines);
        }
    }
}

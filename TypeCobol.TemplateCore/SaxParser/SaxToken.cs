using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Xml;

namespace TypeCobol.TemplateCore.SaxParser
{
    /// <summary>
    /// A Sax Token
    /// </summary>
    public struct SaxToken
    {
        /// <summary>
        ///        
        //// Résumé :
        ////     Élément (par exemple, <item>).
        //Element = 1,
        ////
        //// Résumé :
        ////     Attribut (par exemple, id='123').
        //Attribute = 2,
        ////
        //// Résumé :
        ////     Texte d'un nœud.
        //Text = 3,
        ////
        //// Résumé :
        ////     Section CDATA (par exemple, <![CDATA[my escaped text]]>).
        //CDATA = 4,
        ////
        //// Résumé :
        ////     Référence à une entité (par exemple, &num;).
        //EntityReference = 5,
        ////
        //// Résumé :
        ////     Déclaration d'entité (par exemple, <!ENTITY...>).
        //Entity = 6,
        ////
        //// Résumé :
        ////     Instruction de traitement (par exemple, <?pi test?>).
        //ProcessingInstruction = 7,
        ////
        //// Résumé :
        ////     Commentaire (par exemple, <!-- my comment -->).
        //Comment = 8,
        ////
        //// Résumé :
        ////     Objet document qui, en tant que racine de l'arborescence de documents, permet
        ////     d'accéder à l'intégralité du document XML.
        //Document = 9,
        ////
        //// Résumé :
        ////     Déclaration de type du document, indiquée par la balise suivante (par exemple,
        ////     <!DOCTYPE...>).
        //DocumentType = 10,
        ////
        //// Résumé :
        ////     Fragment de document.
        //DocumentFragment = 11,
        ////
        //// Résumé :
        ////     Notation dans la déclaration de type du document (par exemple, <!NOTATION...>).
        //Notation = 12,
        ////
        //// Résumé :
        ////     Espace blanc entre le balisage.
        //Whitespace = 13,
        ////
        //// Résumé :
        ////     Espace blanc entre le balisage dans un modèle de contenu mixte ou espace
        ////     blanc dans la portée xml:space="preserve".
        //SignificantWhitespace = 14,
        ////
        //// Résumé :
        ////     Balise d'élément de fin (par exemple, </item>).
        //EndElement = 15,
        ////
        //// Résumé :
        ////     Retourné lorsque XmlReader parvient à la fin du remplacement de l'entité,
        ////     à la suite d'un appel à System.Xml.XmlReader.ResolveEntity().
        //EndEntity = 16,
        ////
        //// Résumé :
        ////     Déclaration XML (par exemple, <?xml version='1.0'?>).     
        //XmlDeclaration = 17,
        /// </summary>
        public XmlNodeType type;
        /// <summary>
        /// Token's name
        /// </summary>
        public String name;
        /// <summary>
        /// Token's Value
        /// </summary>
        public String value;
        /// <summary>
        /// Token's text
        /// </summary>
        public string text;
        /// <summary>
        /// Attributes if any (name,value).
        /// </summary>
        public Dictionary<String, String> attributes;
    }
}

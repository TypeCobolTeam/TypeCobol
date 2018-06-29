using System;
using System.Text;
using System.Collections.Generic;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Compiler.Types;

namespace TypeCobol.Test.Types
{
    [TestClass]
    public class PictureStringUnitTest
    {
        [TestMethod]
        public void PictureStringRegExpValidationTest()
        {
            TypeCobol.Compiler.Types.PictureValidator psv = new TypeCobol.Compiler.Types.PictureValidator("$99(45)99.99CRV");
            Assert.IsTrue(!psv.IsValid());
            //psv = new TypeCobol.Compiler.Types.PictureStringValidator("ZZZ.Z9");
            //Assert.IsTrue(!psv.IsValid());

            //Numeric
            string[] numerics = { "9999",
                "S99",
                "S999V9",
                "PPP999",
                "S999PPP",
            };
            for (int i = 0; i < numerics.Length; i++)
            {
                psv = new TypeCobol.Compiler.Types.PictureValidator(numerics[i]);
                Assert.IsTrue(psv.IsValid());
            }


            string[] valids = { "ZZZZ.99",
                        "ZZZZ.99",
                        "++++.99",
                        "Z,ZZZ.99",
                        "$,$$$.99",
                        "$,$$9.99",
                        "$$,$$$.99",
                        "$$9,999.99",
                        "$$$$,$$$.99",
                        "$$$,$$$.$$",
                        "++++.99",
                        "$***,***.99+",
                        "Z,ZZZ.99+",
                        "$,$$$,$$$.99DB"};
            for (int i = 0; i < valids.Length; i++)
            {
                psv = new TypeCobol.Compiler.Types.PictureValidator(valids[i]);
                Assert.IsTrue(psv.IsValid());
            }

            Tuple<string, int, int, bool>[] pics = {
                new Tuple<string, int, int, bool>("9(6)V99", 6, /*whole numbers and*/ 2  /*decimal places*/, false),
            new Tuple<string, int, int, bool>("V999", 0, 3 /*decimal places*/, false),
            new Tuple<string, int, int, bool>("9(4)V9(4)", 4, 4, false),
            new Tuple<string, int, int, bool>("9(5)", 5,  0, false),
            new Tuple<string, int, int, bool>("S9(5)", 5, 0, true /*the sign is remembered*/),
            new Tuple<string, int, int, bool>("S9(4)V9", 4, 1, true),
            new Tuple<string, int, int, bool>("S99V999", 2, 3, true),
            new Tuple<string, int, int, bool>("S999", 3, 0, true)
            };

            for (int i = 0; i < pics.Length; i++)
            {
                psv = new TypeCobol.Compiler.Types.PictureValidator(pics[i].Item1);
                Assert.IsTrue(psv.IsValid());
                Assert.AreEqual(psv.ValidationContext.RealDigits - psv.ValidationContext.Scale, pics[i].Item2);
                Assert.AreEqual(psv.ValidationContext.Scale, pics[i].Item3);
                Assert.AreEqual(psv.ValidationContext.HaveSign, pics[i].Item4);
            }

            string[] ifp_pics =
            {
                "-ZZ9.99",
                "-ZZ9.99",
                "Z,ZZ9-",
                "Z,ZZ9-",
                "Z,ZZ9-",
                "Z,ZZ9-",
                "+ZZ,ZZ9",
                "+ZZ,ZZ9",
                "+Z,ZZ9.99",
                "+Z,ZZZ.99",
                "--,--9.99",
                "--,--9.99",
                "--,---.99",
                "--,---.99",
                "+++,+++.99",
                "+++,+++.99",
                "Z,ZZ9CR",
                "Z,ZZ9.99CR",
                "Z,ZZ9.99DB",
                "Z,ZZ9.99DB",
                "+,+++,999.99",
                "++,+++,+++.+++"
            };

            for (int i = 0; i < ifp_pics.Length; i++)
            {
                psv = new TypeCobol.Compiler.Types.PictureValidator(ifp_pics[i]);
                Assert.IsTrue(psv.IsValid());
            }

            string ex_float_pic = "-9v9(9)E-99";
            psv = new TypeCobol.Compiler.Types.PictureValidator(ex_float_pic);
            Assert.IsTrue(psv.IsValid());
            Assert.IsTrue(psv.ValidationContext.IsExternalFloatSequence());

            ex_float_pic = "-99(9).E-99";
            psv = new TypeCobol.Compiler.Types.PictureValidator(ex_float_pic);
            Assert.IsTrue(psv.IsValid());
            Assert.IsTrue(psv.ValidationContext.IsExternalFloatSequence());

            ex_float_pic = "+VE-99";
            psv = new TypeCobol.Compiler.Types.PictureValidator(ex_float_pic);
            Assert.IsTrue(psv.IsValid());
            Assert.IsTrue(!psv.ValidationContext.IsExternalFloatSequence());
        }

        /// <summary>
        /// Tests from
        /// https://www.ibm.com/support/knowledgecenter/en/SSAE4W_9.1.0/com.ibm.etools.iseries.langref.doc/c0925395267.htm
        /// </summary>
        [TestMethod]
        public void PictureSimpleInsertionTest()
        {
            Tuple<string,int>[] pics =
            {
                new Tuple<string,int>("X(10)/XX", 13),
                new Tuple<string,int>("X(5)BX(7)", 13),
                new Tuple<string,int>("99,B999,B000", 12),
                new Tuple<string,int>("99,999", 6),
                new Tuple<string,int>("GGBBGG", 12)
            };
            for (int i = 0; i < pics.Length; i++)
            {
                TypeCobol.Compiler.Types.PictureValidator psv = new TypeCobol.Compiler.Types.PictureValidator(pics[i].Item1);
                Assert.IsTrue(psv.IsValid());
                PictureType type = new PictureType(psv);
                int len = type.Length;
                Assert.AreEqual(len, pics[i].Item2);
            }
        }

        /// <summary>
        /// Tests from
        /// https://www.ibm.com/support/knowledgecenter/en/SSAE4W_9.1.0/com.ibm.etools.iseries.langref.doc/c0925395267.htm
        /// </summary>
        [TestMethod]
        public void PictureSpecialInsertionEditingTest()
        {
            Tuple<string, int>[] pics =
            {
                new Tuple<string,int>("999.99", 6),
                new Tuple<string,int>("+999.99E+99", 11),
            };
            for (int i = 0; i < pics.Length; i++)
            {
                TypeCobol.Compiler.Types.PictureValidator psv = new TypeCobol.Compiler.Types.PictureValidator(pics[i].Item1);
                Assert.IsTrue(psv.IsValid());
                PictureType type = new PictureType(psv);
                int len = type.Length;
                Assert.AreEqual(len, pics[i].Item2);
            }
        }

        /// <summary>
        /// Tests from
        /// https://www.ibm.com/support/knowledgecenter/en/SSAE4W_9.1.0/com.ibm.etools.iseries.langref.doc/c0925395267.htm
        /// </summary>
        [TestMethod]
        public void PictureZeroSuppressionAndReplacementEditingTest()
        {
            Tuple<string, int>[] pics =
            {
                new Tuple<string,int>("****.**", 7),
                new Tuple<string,int>("*,***.**+", 9),
                new Tuple<string,int>("$Z,ZZZ,ZZZ.ZZCR", 16),
                new Tuple<string,int>("$B*,***,***.**BBDB", 18),
            };
            for (int i = 0; i < pics.Length; i++)
            {
                TypeCobol.Compiler.Types.PictureValidator psv = new TypeCobol.Compiler.Types.PictureValidator(pics[i].Item1);
                Assert.IsTrue(psv.IsValid());
                PictureType type = new PictureType(psv);
                int len = type.Length;
                Assert.AreEqual(len, pics[i].Item2);
            }
        }

        /// <summary>
        /// Test from
        /// http://www.3480-3590-data-conversion.com/article-packed-fields.html
        /// </summary>
        [TestMethod]
        public void PictureWithUsageTest()
        {
            Tuple<string, TypeCobolType.UsageFormat, int>[] pics =
            {
                new Tuple<string,TypeCobolType.UsageFormat,int>("S9(6)V99", TypeCobolType.UsageFormat.None, 8),
                new Tuple<string,TypeCobolType.UsageFormat,int>("S9(6)V99", TypeCobolType.UsageFormat.Comp3, 5),
                new Tuple<string,TypeCobolType.UsageFormat,int>("S9(7)", TypeCobolType.UsageFormat.Comp3, 4),
                new Tuple<string,TypeCobolType.UsageFormat,int>("S9(5)V99", TypeCobolType.UsageFormat.Comp3, 4),
                new Tuple<string,TypeCobolType.UsageFormat,int>("S9(6)", TypeCobolType.UsageFormat.Comp3, 4),
                new Tuple<string,TypeCobolType.UsageFormat,int>("9(7)", TypeCobolType.UsageFormat.Comp3, 4),
                new Tuple<string,TypeCobolType.UsageFormat,int>("9(6)", TypeCobolType.UsageFormat.Comp3, 4),
            };
            for (int i = 0; i < pics.Length; i++)
            {
                TypeCobol.Compiler.Types.PictureValidator psv = new TypeCobol.Compiler.Types.PictureValidator(pics[i].Item1);
                Assert.IsTrue(psv.IsValid());
                PictureType type = new PictureType(psv);
                type.Usage = pics[i].Item2;
                int len = type.Length;
                Assert.AreEqual(len, pics[i].Item3);
            }
        }
    }
}


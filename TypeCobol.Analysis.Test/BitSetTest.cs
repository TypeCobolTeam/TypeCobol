using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace TypeCobol.Analysis.Test
{
    [TestClass]
    public class BitSetTest
    {
        [TestMethod]
        [TestCategory("BitSet")]
        public void BitSetNotTest()
        {
            Util.BitSet bs1 = new Util.BitSet(128);
            for (int i = 0; i < 128; i += 2)
            {
                bs1.Set(i);
            }
            bs1.Not();
            int next = 0;
            while(next < 128)
            {
                int newNext = bs1.NextSetBit(next);
                Assert.AreEqual(newNext % 2, 1);
                Assert.AreEqual(newNext, next + 1);
                next = newNext + 1;
            }
            bs1.Not();
            next = 0;
            while (next <= 126)
            {
                int newNext = bs1.NextSetBit(next);
                Assert.AreEqual(newNext % 2, 0);
                Assert.AreEqual(newNext, next == 0 ? 0 : next + 1);
                next = newNext + 1;
            }
        }

        [TestMethod]
        [TestCategory("BitSet")]
        public void BitSetUnionTest()
        {
            Util.BitSet bs1 = new Util.BitSet(128);
            bs1.Set(10);
            bs1.Set(67);

            Util.BitSet bs2 = new Util.BitSet(128);
            bs1.Set(18);
            bs1.Set(118);

            Util.BitSet bs3 = bs1.Union(bs2);

            Assert.AreEqual(bs3.NextSetBit(0), 10);
            Assert.AreEqual(bs3.NextSetBit(11), 18);
            Assert.AreEqual(bs3.NextSetBit(19), 67);
            Assert.AreEqual(bs3.NextSetBit(69), 118);
            Assert.AreEqual(bs3.NextSetBit(119), -1);
        }

        [TestMethod]
        [TestCategory("BitSet")]
        public void BitSetIntersectionTest0()
        {
            Util.BitSet bs1 = new Util.BitSet(256);
            bs1.Set(10);
            bs1.Set(67);
            bs1.Set(211);

            Util.BitSet bs2 = new Util.BitSet(512);            
            bs2.Set(18);
            bs2.Set(67);
            bs2.Set(118);
            bs2.Set(412);

            Util.BitSet bs3 = bs1.Intersection(bs2);

            Assert.AreEqual(bs3.NextSetBit(0), 67);
            Assert.AreEqual(bs3.NextSetBit(68), -1);
        }

        [TestMethod]
        [TestCategory("BitSet")]
        public void BitSetIntersectionTest1()
        {
            Util.BitSet bs1 = new Util.BitSet(256);
            bs1.Set(10);
            bs1.Set(67);
            bs1.Set(211);

            Util.BitSet bs2 = new Util.BitSet(512);
            bs2.Set(18);
            bs2.Set(67);
            bs2.Set(118);
            bs2.Set(412);

            Util.BitSet bs3 = bs2.Intersection(bs1);

            Assert.AreEqual(bs3.NextSetBit(0), 67);
            Assert.AreEqual(bs3.NextSetBit(68), -1);
        }

        [TestMethod]
        [TestCategory("BitSet")]
        public void BitSetDifferenceTest0()
        {
            Util.BitSet bs1 = new Util.BitSet(256);
            bs1.Set(10);
            bs1.Set(67);
            bs1.Set(211);

            Util.BitSet bs2 = new Util.BitSet(512);
            bs2.Set(18);
            bs2.Set(67);
            bs2.Set(118);
            bs2.Set(412);

            Util.BitSet bs3 = bs1.Difference(bs2);

            Assert.AreEqual(bs3.NextSetBit(0), 10);
            Assert.AreEqual(bs3.NextSetBit(11), 211);
            Assert.AreEqual(bs3.NextSetBit(212), -1);
        }

        [TestMethod]
        [TestCategory("BitSet")]
        public void BitSetDifferenceTest1()
        {
            Util.BitSet bs1 = new Util.BitSet(256);
            bs1.Set(10);
            bs1.Set(67);
            bs1.Set(211);

            Util.BitSet bs2 = new Util.BitSet(512);
            bs2.Set(18);
            bs2.Set(67);
            bs2.Set(118);
            bs2.Set(412);

            Util.BitSet bs3 = bs2.Difference(bs1);

            Assert.AreEqual(bs3.NextSetBit(0), 18);
            Assert.AreEqual(bs3.NextSetBit(19), 118);
            Assert.AreEqual(bs3.NextSetBit(119), 412);
            Assert.AreEqual(bs3.NextSetBit(413), -1);
        }

        [TestMethod]
        [TestCategory("BitSet")]
        public void BitSetXorTest1()
        {
            Util.BitSet bs1 = new Util.BitSet(256);
            bs1.Set(10);
            bs1.Set(67);
            bs1.Set(211);

            Util.BitSet bs2 = new Util.BitSet(256);
            bs2.Set(18);
            bs2.Set(67);
            bs2.Set(118);

            bs2.Xor(bs1);

            Assert.AreEqual(bs2.NextSetBit(0), 10);
            Assert.AreEqual(bs2.NextSetBit(11), 18);
            Assert.AreEqual(bs2.NextSetBit(19), 118);
            Assert.AreEqual(bs2.NextSetBit(119), 211);
            Assert.AreEqual(bs2.NextSetBit(212), -1);
        }
    }
}

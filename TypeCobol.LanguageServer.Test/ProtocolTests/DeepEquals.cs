using System.Collections;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Reflection;
using System.Text;

namespace TypeCobol.LanguageServer.Test.ProtocolTests
{
    internal static class DeepEquals
    {
        private class NullDataException : Exception
        {
            public Stack<string> Path { get; }

            public NullDataException(Stack<string> path)
            {
                Path = path;
            }
        }

        private class NotEqualException : Exception
        {
            public Stack<string> Path { get; }
            public object ExpectedValue { get; }
            public object ActualValue { get; }

            public NotEqualException(Stack<string> path, object expectedValue, object actual)
            {
                Path = path;
                ExpectedValue = expectedValue;
                ActualValue = actual;
            }
        }

        public static void AssertAreEqual(object expected, object actual)
        {
            try
            {
                CheckEquals(expected, actual);
            }
            catch (NullDataException nullDataException)
            {
                var error = new StringBuilder();
                error.AppendLine("Potential missing data found:");
                error.AppendLine(string.Join(" -> ", nullDataException.Path.Reverse()));
                Assert.Fail(error.ToString());
            }
            catch (NotEqualException notEqualException)
            {
                var error = new StringBuilder();
                error.AppendLine("Objects are not equal:");
                error.AppendLine(string.Join(" -> ", notEqualException.Path.Reverse()));
                error.AppendLine($"Expected: {ToString(notEqualException.ExpectedValue)}");
                error.AppendLine($"Actual: {ToString(notEqualException.ActualValue)}");
                Assert.Fail(error.ToString());
            }

            static string ToString(object obj) => obj == null ? "<NULL>" : obj.ToString();
        }

        private static void CheckEquals(object expected, object actual)
        {
            var stack = new Stack<string>();
            Compare(".", expected, actual);

            void Compare(string name, object expectedFieldValue, object actualFieldValue)
            {
                stack.Push(name);

                if (ReferenceEquals(expectedFieldValue, actualFieldValue))
                {
                    if (expectedFieldValue == null)
                    {
                        throw new NullDataException(stack);
                    }

                    stack.Pop();
                    return;
                }

                if (expectedFieldValue == null || actualFieldValue == null)
                {
                    throw new NotEqualException(stack, expectedFieldValue, actualFieldValue);
                }

                if (expectedFieldValue.GetType() != actualFieldValue.GetType())
                {
                    throw new NotEqualException(stack, expectedFieldValue, actualFieldValue);
                }

                var type = expectedFieldValue.GetType();
                if (type.IsPrimitive || type == typeof(string))
                {
                    if (!expectedFieldValue.Equals(actualFieldValue))
                    {
                        throw new NotEqualException(stack, expectedFieldValue, actualFieldValue);
                    }
                }
                else if (typeof(IEnumerable).IsAssignableFrom(type))
                {
                    var expectedFieldValues = ((IEnumerable)expectedFieldValue).Cast<object>().ToArray();
                    var actualFieldValues = ((IEnumerable)actualFieldValue).Cast<object>().ToArray();
                    if (expectedFieldValues.Length != actualFieldValues.Length)
                    {
                        throw new NotEqualException(stack, expectedFieldValue, actualFieldValue);
                    }

                    int length = expectedFieldValues.Length;
                    for (int i = 0; i < length; i++)
                    {
                        var expectedItemValue = expectedFieldValues[i];
                        var actualItemValue = actualFieldValues[i];
                        Compare($"Item#{i}", expectedItemValue, actualItemValue);
                    }
                }
                else if (type != typeof(object))
                {
                    foreach (var childField in type.GetFields(BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic))
                    {
                        var expectedChildFieldValue = childField.GetValue(expectedFieldValue);
                        var actualChildFieldValue = childField.GetValue(actualFieldValue);
                        Compare(childField.Name, expectedChildFieldValue, actualChildFieldValue);
                    }
                }

                stack.Pop();
            }
        }
    }
}

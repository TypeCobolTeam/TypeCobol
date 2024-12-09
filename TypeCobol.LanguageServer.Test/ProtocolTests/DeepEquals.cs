using System.Collections;
using System.Reflection;
using System.Text;

namespace TypeCobol.LanguageServer.Test.ProtocolTests
{
    /// <summary>
    /// Utility class to compare two objects by their data.
    /// </summary>
    internal static class DeepEquals
    {
        public class NullDataException : Exception
        {
            public Stack<string> Path { get; }

            public NullDataException(Stack<string> path)
            {
                Path = path;
            }

            public override string Message
            {
                get
                {
                    var error = new StringBuilder();
                    error.AppendLine("Potential missing data found:");
                    error.AppendLine(string.Join(" -> ", Path.Reverse()));
                    return error.ToString();
                }
            }
        }

        public class NotEqualException : Exception
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

            public override string Message
            {
                get
                {
                    var error = new StringBuilder();
                    error.AppendLine("Objects are not equal:");
                    error.AppendLine(string.Join(" -> ", Path.Reverse()));
                    error.AppendLine($"Expected: {ValueToString(ExpectedValue)}");
                    error.AppendLine($"Actual: {ValueToString(ActualValue)}");
                    return error.ToString();

                    static string ValueToString(object obj) => obj == null ? "<NULL>" : obj.ToString();
                }
            }
        }

        public static void CheckAreEqual(object expected, object actual)
        {
            // Traversal of the objects, the stack tracks the current field being compared
            var stack = new Stack<string>();
            Compare(".", expected, actual);

            void Compare(string name, object expectedFieldValue, object actualFieldValue)
            {
                stack.Push(name);

                if (ReferenceEquals(expectedFieldValue, actualFieldValue))
                {
                    // Same instance or both null.
                    if (expectedFieldValue == null)
                    {
                        // This could be a missing data from a serialization/deserialization mismatch
                        throw new NullDataException(stack);
                    }

                    // Ok
                    stack.Pop();
                    return;
                }

                if (expectedFieldValue == null || actualFieldValue == null)
                {
                    // One null but not the other
                    throw new NotEqualException(stack, expectedFieldValue, actualFieldValue);
                }

                if (expectedFieldValue.GetType() != actualFieldValue.GetType())
                {
                    // Type mismatch
                    throw new NotEqualException(stack, expectedFieldValue, actualFieldValue);
                }

                var type = expectedFieldValue.GetType();
                if (type.IsPrimitive || type == typeof(string))
                {
                    if (!expectedFieldValue.Equals(actualFieldValue))
                    {
                        // Different values
                        throw new NotEqualException(stack, expectedFieldValue, actualFieldValue);
                    }
                }
                else if (typeof(IEnumerable).IsAssignableFrom(type))
                {
                    // Try comparing collection of values
                    var expectedFieldValues = ((IEnumerable)expectedFieldValue).Cast<object>().ToArray();
                    var actualFieldValues = ((IEnumerable)actualFieldValue).Cast<object>().ToArray();
                    if (expectedFieldValues.Length != actualFieldValues.Length)
                    {
                        // Not the same number of elements
                        throw new NotEqualException(stack, expectedFieldValue, actualFieldValue);
                    }

                    // Compare each element. Note that this assumes the elements are in same order which may not be a requirement.
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
                    // Compare each instance field of the objects
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

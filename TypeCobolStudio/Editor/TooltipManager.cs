using ICSharpCode.AvalonEdit.Document;
using ICSharpCode.AvalonEdit.Rendering;
using System;
using System.Linq;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;

namespace TypeCobolStudio.Editor
{
    internal class TooltipManager
    {
        // Current tooltip displayed on the editor
        ToolTip toolTip;

        // Editor context
        TypeCobolEditor editor;
        TextView textView;
        ErrorMarker errorMarker;

        public TooltipManager(TypeCobolEditor editor, ErrorMarker errorMarker)
        {
            this.editor = editor;
            this.textView = editor.TextArea.TextView;
            this.errorMarker = errorMarker;
        }
        
        public void MouseHover(object sender, MouseEventArgs e)
        {
            var pos = textView.GetPositionFloor(e.GetPosition(textView) + textView.ScrollOffset);
            bool inDocument = pos.HasValue;
            if (inDocument)
            {
                TextLocation logicalPosition = pos.Value.Location;
                int offset = textView.Document.GetOffset(logicalPosition);

                var markersAtOffset = errorMarker.GetMarkersAtOffset(offset);
                ErrorMarker.TextMarker markerWithToolTip = markersAtOffset.FirstOrDefault(marker => marker.ToolTip != null);

                if (markerWithToolTip != null)
                {
                    if (toolTip == null)
                    {
                        toolTip = new ToolTip();
                        toolTip.Closed += ToolTipClosed;
                        toolTip.PlacementTarget = editor;
                        toolTip.Content = new TextBlock
                        {
                            Text = markerWithToolTip.ToolTip,
                            TextWrapping = TextWrapping.Wrap
                        };
                        toolTip.IsOpen = true;
                        e.Handled = true;
                    }
                }
            }
        }

        public void MouseHoverStopped(object sender, MouseEventArgs e)
        {
            if (toolTip != null)
            {
                toolTip.IsOpen = false;
                e.Handled = true;
            }
        }

        public void VisualLinesChanged(object sender, EventArgs e)
        {
            if (toolTip != null)
            {
                toolTip.IsOpen = false;
            }
        }

        private void ToolTipClosed(object sender, RoutedEventArgs e)
        {
            toolTip = null;
        }
    }
}

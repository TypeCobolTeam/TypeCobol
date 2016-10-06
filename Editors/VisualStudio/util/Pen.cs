using System.Windows;
using System.Windows.Media;

namespace TypeCobol.Editor.util
{
    public class Pens
    {
        public static Pen CompileError = CreateErrorPen(Colors.Blue);
        public static Pen SyntaxError = CreateErrorPen(Colors.Red);
        public static Pen Warning = CreateErrorPen(Colors.Green);

        public static Pen CreateRainbowPen(DashStyle style = null)
        {
            var pen = new Pen();
            pen.Brush = new LinearGradientBrush(Colors.Yellow, Colors.Red, new Point(0, 0.5), new Point(1, 0.5));
            pen.Brush.Opacity = 0.5;
            pen.Thickness = 1.5;
            pen.DashStyle = (style != null ? style : DashStyles.Dash);
            pen.Freeze();
            return pen;
        }

        public static Pen CreateErrorPen(Color color)
        {
            var geometry = new StreamGeometry();
            using (var context = geometry.Open())
            {
                context.BeginFigure(new Point(-1, 0), false, false);
                context.PolyLineTo(new[] {
                        new Point(-0.5, 0.4),
                        new Point(0, 0),
                        new Point(0.5, -0.4),
                        new Point(1, 0),
                    }, true, true);
            }
            var brushPattern = new GeometryDrawing
            {
                Pen = new Pen(new SolidColorBrush(color), 0.4),
                Geometry = geometry
            };
            var brush = new DrawingBrush(brushPattern)
            {
                TileMode = TileMode.Tile,
                Viewport = new Rect(-1, -1, 2, 2),
                ViewportUnits = BrushMappingMode.Absolute,
                Viewbox = new Rect(-1, -1, 2, 2),
                ViewboxUnits = BrushMappingMode.Absolute,
            };
            var pen = new Pen(brush, 3.0);
            pen.Freeze();
            return pen;
        }
    }
}

package typecobol.editors.eclipse;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Display;

public class ColorMap {

	protected Map<RGB,Color> colors = new HashMap<RGB,Color>(10);

	public void dispose() {
		for (Color c : colors.values()) c.dispose();
	}
	public Color getColor(final RGB rgb) {
		Color color = colors.get(rgb);
		if (color == null) {
			color = new Color(Display.getCurrent(), rgb);
			colors.put(rgb, color);
		}
		return color;
	}
}

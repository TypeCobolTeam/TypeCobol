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
	public Color getColor(final String rgb) {
		return getColor(hex2rgb(rgb));
	}

	public static RGB hex2rgb(final String code) {
	    final java.awt.Color color = new java.awt.Color(
	            Integer.valueOf(code.substring(1,3), 16),
	            Integer.valueOf(code.substring(3,5), 16),
	            Integer.valueOf(code.substring(5,7), 16));
	    return new RGB(color.getRed(), color.getGreen(), color.getBlue());
	}
}

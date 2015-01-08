/* Example of use:

access piechart;
unitsize (70);

int[] values = {1, 2, 3, 2};
string[] labels = {"a", "b", "c", "very long"};

add(piechart.pie(values, labels, 1.2, green, labelcolor=blue));
*/

picture pie (int[] values, string[] labels={}, real distance = 0.5, pen color = rgb(1,1,1), pen labelcolor = rgb(0, 0, 0)) {
	picture opic;
	// unitsize(100);
	int total = 0;
	for (int i: values) {
		total += i;
	}

	if (total != 0) {
		real last = 0;
		for (int i = 0; i < values.length; ++i) {
			real current = last+values[i]*360/total;
			filldraw(opic, arc((0,0), 1, last, current) -- (0,0) -- cycle, values[i]/total * color);
			if (labels.length == values.length) {
				// rotate() 0 angle is upwards. We shift it to match the trigonometric circle.
				label (opic, labels[i], rotate((current+last)/2-90) * (0, distance), labelcolor);
			}
			last = current;
		}
	}
	return opic;
}


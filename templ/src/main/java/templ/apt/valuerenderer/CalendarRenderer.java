package templ.apt.valuerenderer;

import java.text.SimpleDateFormat;
import java.util.Calendar;

public class CalendarRenderer extends ValueRenderer<Calendar> {

    private static SimpleDateFormat formatter = new SimpleDateFormat("yyyy-MM-dd");

    @Override
    public Class<Calendar> getType() {
        return Calendar.class;
    }

    @Override
    public String render(Calendar o) {
        return formatter.format(o.getTime());
    }
}
package templ.apt.valuerenderer;

import java.text.SimpleDateFormat;
import java.util.Date;

public class DateRenderer extends ValueRenderer<Date> {

    private static SimpleDateFormat formatter = new SimpleDateFormat("yyyy-MM-dd");

    @Override
    public Class<Date> getType() {
        return Date.class;
    }

    @Override
    public String render(Date o) {
        return formatter.format(o);
    }
}
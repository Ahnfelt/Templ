package templ.apt.valuerenderer;

public class StringRenderer extends ValueRenderer<String> {

    @Override
    public Class<String> getType() {
        return String.class;
    }

    @Override
    public String render(String o) {
        return o;
    }
}

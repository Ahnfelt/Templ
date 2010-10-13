package templ.apt.valuerenderer;

public class IntegerRenderer extends ValueRenderer<Integer> {

    @Override
    public Class<Integer> getType() {
        return Integer.class;
    }

    @Override
    public String render(Integer o) {
        return o.toString();
    }
}
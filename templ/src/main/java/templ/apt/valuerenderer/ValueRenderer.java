package templ.apt.valuerenderer;

public abstract class ValueRenderer<T> {

    public abstract Class<T> getType();

    public abstract String render(T o);
}

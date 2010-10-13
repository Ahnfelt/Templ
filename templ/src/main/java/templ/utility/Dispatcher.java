package templ.utility;

public interface Dispatcher<T, R> {
    public R dispatch(T value);
}

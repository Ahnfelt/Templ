package templ.utility;

public interface Dispatcher2<T1, T2, R> {
    public R dispatch(T1 v1, T2 v2);
}
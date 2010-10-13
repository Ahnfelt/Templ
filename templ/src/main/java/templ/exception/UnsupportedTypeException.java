package templ.exception;

public class UnsupportedTypeException extends RuntimeException {
    public UnsupportedTypeException() {
    }

    public UnsupportedTypeException(String s) {
        super(s);
    }

    public UnsupportedTypeException(String s, Throwable throwable) {
        super(s, throwable);
    }

    public UnsupportedTypeException(Throwable throwable) {
        super(throwable);
    }
}
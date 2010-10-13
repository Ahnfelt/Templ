package templ.exception;

public class ConversionError extends RuntimeException {

    public ConversionError(String s) {
        super(s);
    }

    public ConversionError(String s, Throwable throwable) {
        super(s, throwable);
    }
}

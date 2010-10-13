package templ.utility;

/** An unchecked exception that is thrown when there is no other sensible option. */
public class FileException extends RuntimeException {
    public FileException() {
    }

    public FileException(String s) {
        super(s);
    }

    public FileException(String s, Throwable throwable) {
        super(s, throwable);
    }

    public FileException(Throwable throwable) {
        super(throwable);
    }
}

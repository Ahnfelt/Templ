package templ.exception;

public class TemplateException extends RuntimeException {
    public TemplateException() {
    }

    public TemplateException(String s) {
        super(s);
    }

    public TemplateException(String s, Throwable throwable) {
        super(s, throwable);
    }

    public TemplateException(Throwable throwable) {
        super(throwable);
    }
}

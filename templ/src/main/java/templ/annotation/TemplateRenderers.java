package templ.annotation;

import templ.apt.valuerenderer.ValueRenderer;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
public @interface TemplateRenderers {
    public abstract Class<? extends ValueRenderer>[] value();
}
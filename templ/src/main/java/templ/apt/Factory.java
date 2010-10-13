package templ.apt;

import com.sun.mirror.apt.*;
import com.sun.mirror.declaration.*;
import templ.annotation.Template;

import java.util.*;

import static java.util.Collections.*;

public class Factory implements AnnotationProcessorFactory {

    private static final Collection<String> supportedAnnotations
            = unmodifiableCollection(Arrays.asList(Template.class.getName()));

    // No supported options
    private static final Collection<String> supportedOptions = emptySet();

    public Collection<String> supportedAnnotationTypes() {
        return supportedAnnotations;
    }

    public Collection<String> supportedOptions() {
        return supportedOptions;
    }

    public AnnotationProcessor getProcessorFor(Set<AnnotationTypeDeclaration> atds,
                                               AnnotationProcessorEnvironment env) {
        return new TemplAnnotationProcessor(env);
    }

}

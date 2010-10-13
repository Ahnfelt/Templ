package templ.utility;

import java.net.URL;

public class ResourceLocator {

    public static String locate(String file) {
        URL resource = ResourceLocator.class.getClassLoader().getResource(file);
        if (resource == null) {
            throw new ResourceLocatorException(file);
        } else {
            return resource.getPath();
        }
    }

    private static class ResourceLocatorException extends RuntimeException {
        public ResourceLocatorException(String file) {
            super("Cannot find resource: "+file);   
        }


    }
}

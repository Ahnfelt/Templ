package templ.utility;

import java.net.URL;

public class ResourceLocator {

    public static String locate(String file) {
        URL resource = ResourceLocator.class.getClassLoader().getResource(file);
        if (resource == null) {
            //System.out.println(file+" NOT FOUND");
            return null;
        } else {
            //System.out.println("  FOUND URL "+file+" at "+resource);
            return resource.getPath();
        }
    }
}

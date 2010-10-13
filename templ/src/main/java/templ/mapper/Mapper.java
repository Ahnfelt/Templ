package templ.mapper;

import java.util.SortedSet;
import java.util.TreeSet;

public abstract class Mapper {

    private static SortedSet<String> methodBlackList = new TreeSet<String>();

    static {
        methodBlackList.add("getClass");
    }

    public static boolean isBlacklisted(String getMethodName) {
        return methodBlackList.contains(getMethodName);
    }

}

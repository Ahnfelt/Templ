package templ.utility;

import java.lang.reflect.Method;

public class JavaBeans {

    public JavaBeans(Method method) {
    }

    public static boolean isGetMethod(Method method) {
        if (method.getParameterTypes().length > 0) return false;
        String methodName = method.getName();
        if (methodName.length() < 4 || ! methodName.startsWith("get")) return false;
        Character fourthChar = methodName.charAt(3);
        return Character.isUpperCase(fourthChar);
        
    }

    public static String getterFieldName(Method method) {
        String methodName = method.getName();
        return methodName.substring(3,4).toLowerCase() + methodName.substring(4);
    }
}

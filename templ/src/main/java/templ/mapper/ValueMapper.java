package templ.mapper;

import templ.exception.ConversionError;
import templ.exception.TemplateException;
import templ.utility.JavaBeans;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Collection;
import java.util.Iterator;

public class ValueMapper extends Mapper {

    public static String convert(Object value) {
        if (value == null) throw new TemplateException("Null where it shouldn't!");
        else if (value instanceof String) return convertString((String) value);
        else if (value instanceof Collection) return convertCollection((Collection) value);
        else return convertRecord(value);
    }

    private static String convertNull() {
        return "()";
    }

    private static String convertString(final String value) {
        String valueEscaped = value.replaceAll("([@$|{}])", "\\$$0");
        return "{"+valueEscaped+"}";
    }

    private static String convertCollection(final Collection value) {
        StringBuilder builder = new StringBuilder();
        builder.append("[");
        Iterator iterator = value.iterator();
        while(iterator.hasNext()) {
            Object element = iterator.next();
            builder.append(convert(element));
            if(iterator.hasNext()) builder.append(",");
        }
        builder.append("]");
        return builder.toString();
    }

    private static String convertRecord(final Object value) {
        StringBuilder builder = new StringBuilder();
        builder.append("(");
        for (Method method: value.getClass().getMethods()) {
            if (!JavaBeans.isGetMethod(method)) continue;
            String methodName = method.getName();
            if (isBlacklisted(methodName)) continue;
            String fieldName = JavaBeans.getterFieldName(method);
            Object fieldValue;
            try {
                fieldValue = method.invoke(value);
            } catch (IllegalAccessException e) {
                throw new ConversionError("Failed to invoke get-method "+
                    methodName+" on "+value.getClass().getName(), e);
            } catch (InvocationTargetException e) {
                throw new ConversionError("Failed to invoke get-method "+
                methodName+" on "+value.getClass().getName(), e);
            }
            if(fieldValue != null) {
                builder.append(fieldName);
                builder.append(":");
                builder.append(convert(fieldValue));
                builder.append(",");
            }
        }

        int last = builder.length()-1;
        if (builder.charAt(last) == ',') {
            builder.deleteCharAt(last);
        }
        builder.append(")");
        return builder.toString();
    }
}

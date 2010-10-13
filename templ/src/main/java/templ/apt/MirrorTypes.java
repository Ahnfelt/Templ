package templ.apt;

import com.sun.mirror.declaration.AnnotationMirror;
import com.sun.mirror.declaration.ClassDeclaration;
import com.sun.mirror.declaration.MethodDeclaration;
import com.sun.mirror.declaration.TypeDeclaration;
import com.sun.mirror.type.ClassType;
import com.sun.mirror.type.DeclaredType;
import com.sun.mirror.type.InterfaceType;
import com.sun.mirror.type.TypeMirror;

import java.lang.annotation.Annotation;
import java.util.*;


/**
 * Static utility library for Mirror types and declarations 
 */
public abstract class MirrorTypes {

    public static boolean isDeclaring(DeclaredType declaredType, Class clazz) {
        return isDeclaring(declaredType.getDeclaration(), clazz);
    }

    public static boolean isDeclaring(TypeDeclaration typeDeclaration, Class clazz) {
        return typeDeclaration.getQualifiedName().equals(clazz.getName());
    }

    public static boolean isString(TypeMirror type) {
        return type instanceof DeclaredType && isDeclaring((DeclaredType) type, String.class);
    }

    public static boolean isCollection(DeclaredType type) {
        return isDeclaring(type, Collection.class);
    }

    public static boolean isCollectionTransitive(DeclaredType type) {
        if (isCollection(type)) return true;
        for (InterfaceType interfaceType: type.getSuperinterfaces()) {
            if (isCollectionTransitive(interfaceType)) return true;
        }
        return false;
    }

    public static boolean isGetMethod(MethodDeclaration methodDeclaration) {
        if ( ! methodDeclaration.getParameters().isEmpty()) return false;
        String methodName = methodDeclaration.getSimpleName();
        if (methodName.length() < 4 || ! methodName.startsWith("get")) return false;
        Character fourthChar = methodName.charAt(3);
        return Character.isUpperCase(fourthChar);
    }

    public static String getterFieldName(String getMethodName) {
        return getMethodName.substring(3,4).toLowerCase() + getMethodName.substring(4);
    }

    public static String getterFieldName(MethodDeclaration methodDeclaration) {
        return getterFieldName(methodDeclaration.getSimpleName());
    }

    public static Map<String, List<MethodDeclaration>> getSubGetMethods(
            TypeDeclaration declaration, Map<String, List<TypeDeclaration>> subDeclarationMap) {
        Map<String, List<MethodDeclaration>> map = new HashMap<String, List<MethodDeclaration>>();
        collectSubGetMethods(declaration, subDeclarationMap, map);
        return map;

    }

    public static void collectSubGetMethods(TypeDeclaration declaration, 
                                            Map<String, List<TypeDeclaration>> subDeclarationMap,
                                            Map<String, List<MethodDeclaration>> map) {
        // Collect from sub classes
        List<TypeDeclaration> subDeclarations = subDeclarationMap.get(declaration.getQualifiedName());
        for (TypeDeclaration subDeclaration: subDeclarations) {
            collectSubGetMethods(subDeclaration, subDeclarationMap, map);
        }

        // Collect from actual class
        collectActualGetMethods(declaration, map);
    }

    public static Map<String, List<MethodDeclaration>> getSuperGetMethods(TypeDeclaration declaration) {
        Map<String, List<MethodDeclaration>> map = new HashMap<String, List<MethodDeclaration>>();
        collectSuperGetMethods(declaration, map);
        return map;
    }

    public static void collectSuperGetMethods(TypeDeclaration declaration, Map<String, List<MethodDeclaration>> map) {
        // Collect from super class
        if (declaration instanceof ClassDeclaration) {
            ClassType extendsType = ((ClassDeclaration)declaration).getSuperclass();
            if (extendsType != null) {
                collectSuperGetMethods(extendsType.getDeclaration(), map);
            }
        }
        // Collect from super interfaces
        Collection<InterfaceType> interfaceTypes = declaration.getSuperinterfaces();
        for (InterfaceType interfaceType: interfaceTypes) {
            collectSuperGetMethods(interfaceType.getDeclaration(), map);
        }
        // Collect from actual class
        collectActualGetMethods(declaration, map);
    }

    private static void collectActualGetMethods(TypeDeclaration declaration, Map<String, List<MethodDeclaration>> map) {
        for (MethodDeclaration method: declaration.getMethods()) {
            if (! MirrorTypes.isGetMethod(method)) continue;
            String name = method.getSimpleName();
            List<MethodDeclaration> methods = map.get(name);
            if (methods == null) {
                methods = new ArrayList<MethodDeclaration>();
                map.put(name, methods);
            }
            methods.add(method);
        }

    }

    public static boolean hasAnnotation(Class<? extends Annotation> annotationClass, List<MethodDeclaration> methods) {
        for (MethodDeclaration method: methods) {
            for (AnnotationMirror annotationMirror: method.getAnnotationMirrors()) {
                if (isDeclaring(annotationMirror.getAnnotationType(), annotationClass)) return true;
            }
        }
        return false;
    }
}

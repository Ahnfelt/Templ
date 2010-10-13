package templ.mapper;

import com.sun.mirror.declaration.MethodDeclaration;
import com.sun.mirror.declaration.TypeDeclaration;
import com.sun.mirror.type.*;
import com.sun.mirror.util.SimpleTypeVisitor;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import templ.apt.MirrorTypes;
import templ.exception.UnsupportedTypeException;
import templ.mapper.ast.*;

import java.util.*;

public class TypeMapper extends Mapper {

    private final Map<String, List<TypeDeclaration>> subDeclarationMap;
    private final Set<String> seenClassNames = new HashSet<String>();

    public TypeMapper(Map<String, List<TypeDeclaration>> subDeclarationMap) {
        this.subDeclarationMap = subDeclarationMap;
    }

    public TemplType convert(TypeMirror type) {
        seenClassNames.clear();
        return convertInternal(type);
    }

    private TemplType convertInternal(TypeMirror type) {
        ToTemplTypeVisitor visitor = new ToTemplTypeVisitor();
        type.accept(visitor);
        return visitor.templType;
    }

    private TemplType convertString() {
        return new TemplTypeString();
    }

    private TemplType convertCollection(DeclaredType type) {
        Collection<TypeMirror> elementTypes = type.getActualTypeArguments();
        TemplType elementTemplType;
        if (elementTypes.isEmpty()) {
            elementTemplType = new TemplTypeRecord();
        } else {
            elementTemplType =  convertInternal(elementTypes.iterator().next());
        }
        return new TemplTypeList(elementTemplType);
    }

    private TemplType convertRecord(DeclaredType type) {
        String declarationName = type.getDeclaration().getQualifiedName();
        if (seenClassNames.contains(declarationName))
            throw new UnsupportedTypeException(
                "Cyclic types are not allowed for template arguments: "+declarationName);
        seenClassNames.add(declarationName);

        TemplTypeRecord templTypeRecord = new TemplTypeRecord();

        // Add fields for the actual class and super classes
        Map<String, List<MethodDeclaration>> superMethods = MirrorTypes.getSuperGetMethods(type.getDeclaration());
        for (List<MethodDeclaration> methodDeclarations: superMethods.values()) {
            MethodDeclaration methodDeclaration = methodDeclarations.get(0); // This should always exists!
            if (isBlacklisted(methodDeclaration.getSimpleName())) continue;
            String fieldName = MirrorTypes.getterFieldName(methodDeclaration);
            TemplType fieldType = convertInternal(methodDeclaration.getReturnType());
            Boolean nullable = !MirrorTypes.hasAnnotation(NotNull.class, methodDeclarations) &&
                    MirrorTypes.hasAnnotation(Nullable.class, methodDeclarations);
            templTypeRecord.addField(fieldName, new TemplNullableType(fieldType, nullable));
        }

        // Add field from sub classes. Equally named get-methods declared in some or all
        // of the sub classes (but none of the super classes or itself) are included as
        // nullable field if their return type have a common super type.
        Map<String, List<MethodDeclaration>> subMethods = 
            MirrorTypes.getSubGetMethods(type.getDeclaration(), subDeclarationMap);
        Map<String, Stack<TemplType>> ghostCandidateListMap = new HashMap<String, Stack<TemplType>>();
        for (Map.Entry<String, List<MethodDeclaration>> entry: subMethods.entrySet()) {
            String methodName = entry.getKey();
            if (superMethods.containsKey(methodName)) continue;
            List<MethodDeclaration> methodDeclarations = entry.getValue();
            Stack<TemplType> ghostCandidateList = new Stack<TemplType>();
            for (MethodDeclaration methodDeclaration: methodDeclarations) {
                TemplType fieldType = convertInternal(methodDeclaration.getReturnType());
                ghostCandidateList.add(fieldType);
            }
            ghostCandidateListMap.put(methodName, ghostCandidateList);
        }
        candidateLoop:
        for (Map.Entry<String, Stack<TemplType>> entry: ghostCandidateListMap.entrySet()) {
            String fieldName = MirrorTypes.getterFieldName(entry.getKey());
            Stack<TemplType> ghostTypes = entry.getValue(); // there should be at least one element
            while (ghostTypes.size() > 1) {
                TemplType t1 = ghostTypes.pop();
                TemplType t2 = ghostTypes.pop();
                TemplType t = t1.general(t2);
                if (t == null) continue candidateLoop;
                ghostTypes.push(t);
            }
            templTypeRecord.addField(fieldName, new TemplNullableType(ghostTypes.get(0), true));
        }

        seenClassNames.remove(declarationName);
        return templTypeRecord;
    }

    private class ToTemplTypeVisitor extends SimpleTypeVisitor {
        public TemplType templType;

        @Override
        public void visitClassType(ClassType classType) {
            if (MirrorTypes.isString(classType)) {
                templType = convertString();
            }
            else if (MirrorTypes.isCollectionTransitive(classType)) {
                templType = convertCollection(classType);
            }
            else {
                templType = convertRecord(classType);
            }
        }

        @Override
        public void visitInterfaceType(InterfaceType interfaceType) {
            if (MirrorTypes.isCollectionTransitive(interfaceType)) {
                templType = convertCollection(interfaceType);
            }
            else {
                templType = convertRecord(interfaceType);
            }
        }

        @Override
        public void visitPrimitiveType(PrimitiveType primitiveType) {
            failUnsupportedType("primitive");
        }

        @Override
        public void visitVoidType(VoidType voidType) {
            failUnsupportedType("void");
        }

        @Override
        public void visitEnumType(EnumType enumType) {
            failUnsupportedType("enum");
        }

        @Override
        public void visitAnnotationType(AnnotationType annotationType) {
            failUnsupportedType("annotation");
        }

        @Override
        public void visitArrayType(ArrayType arrayType) {
            failUnsupportedType("array");
        }

        @Override
        public void visitTypeVariable(TypeVariable typeVariable) {
            failUnsupportedType("type variables");
        }

        @Override
        public void visitWildcardType(WildcardType wildcardType) {
            failUnsupportedType("wildcard");
        }

        /*@Override
        public void visitTypeMirror(TypeMirror typeMirror) {
            fail("Internal error: Too general case reached.");
        }

        @Override
        public void visitReferenceType(ReferenceType referenceType) {
            fail("Internal error: Too general case reached.");
        }

        @Override
        public void visitDeclaredType(DeclaredType declaredType) {
            fail("Internal error: Too general case reached.");
        }*/
    }

    private void fail(String reason) {
        throw new RuntimeException(reason);
    }

    private void failUnsupportedType(String unsupportedTypeName) {
        fail("Conversion of "+unsupportedTypeName+" types not supported.");
    }
}

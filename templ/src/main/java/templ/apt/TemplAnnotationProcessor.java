package templ.apt;

import com.sun.mirror.apt.AnnotationProcessor;
import com.sun.mirror.apt.AnnotationProcessorEnvironment;
import com.sun.mirror.declaration.*;
import com.sun.mirror.type.*;
import com.sun.mirror.util.SimpleDeclarationVisitor;
import templ.annotation.Template;
import templ.mapper.TypeMapper;
import templ.utility.Files;
import templ.utility.ResourceLocator;
import templ.exception.TemplateException;
import templ.utility.Strings;

import java.util.*;
import java.io.InputStream;
import java.io.IOException;

import static com.sun.mirror.util.DeclarationVisitors.NO_OP;
import static com.sun.mirror.util.DeclarationVisitors.getDeclarationScanner;

public class TemplAnnotationProcessor implements AnnotationProcessor {

    private final AnnotationProcessorEnvironment env;

    public TemplAnnotationProcessor(AnnotationProcessorEnvironment env) {
        this.env = env;
    }

    public void process() {
        // Build subDeclarationMap
        SubDeclarationMapVisitor subDeclarationMapVisitor = new SubDeclarationMapVisitor();
        for (TypeDeclaration typeDeclaration : env.getSpecifiedTypeDeclarations()) {
            typeDeclaration.accept(getDeclarationScanner(subDeclarationMapVisitor, NO_OP));
        }

        // Collect render method declarations from render stub interfaces
        CollectRenderStubsVisitor collector = new CollectRenderStubsVisitor();
        for (TypeDeclaration typeDeclaration : env.getSpecifiedTypeDeclarations()) {
            typeDeclaration.accept(getDeclarationScanner(collector, NO_OP));
        }

        // Type check render method declarations
        TypeMapper typeMapper = new TypeMapper(subDeclarationMapVisitor.subDeclarationMap);
        for (MethodDeclaration methodDeclaration: collector.methodDeclarations) {
            // There should be exactly one parameter.
            ParameterDeclaration parameter = methodDeclaration.getParameters().iterator().next();
            Template templateAnnotation = methodDeclaration.getAnnotation(Template.class);
            String templateFileName = templateAnnotation.value();
            TypeMirror inputType = parameter.getType();

            try {
                String type = typeMapper.convert(inputType).toString();
                String templateFile = ResourceLocator.locate(templateFileName);
                String executableFile = ResourceLocator.locate("Templ");
                String[] command = {executableFile, "check", templateFile, type};
                System.out.println("inputType: " + inputType);
                System.out.println("EXEC: "+ Strings.separateBy(command, " "));
                Process process = Runtime.getRuntime().exec(command, null, null);
                String error = Files.read(process.getErrorStream());
                int result = process.waitFor();
                if(result != 0) {
                    throw new TemplateException("Calling " + methodDeclaration + " with " + inputType + 
                            " (equivalent to " + type + ") gave the following error: " + error);
                }
            } catch(InterruptedException e) {
                throw new TemplateException(e);
            } catch(IOException e) {
                throw new TemplateException(e);
            }
        }
    }


    private class SubDeclarationMapVisitor extends SimpleDeclarationVisitor {

        public Map<String, List<TypeDeclaration>> subDeclarationMap = new HashMap<String, List<TypeDeclaration>>();

        @Override
        public void visitTypeDeclaration(TypeDeclaration subDeclaration) {
            getSubDeclarationList(subDeclaration);

            // Add this declaration as a sub-declaration for a potential super class.
            if (subDeclaration instanceof ClassDeclaration) {
                ClassType superType = ((ClassDeclaration) subDeclaration).getSuperclass();
                if (superType != null) {
                    addSubToSuper(subDeclaration, superType);
                }
            }

            // Add this declaration as a sub-declaration for all super interfaces.
            for (InterfaceType superType: subDeclaration.getSuperinterfaces()) {
                addSubToSuper(subDeclaration, superType);
            }
        }

        private List<TypeDeclaration> getSubDeclarationList(TypeDeclaration declaredType) {
            String name = declaredType.getQualifiedName();
            List<TypeDeclaration> subDeclarations = subDeclarationMap.get(name);
            if (subDeclarations == null) {
                subDeclarations = new ArrayList<TypeDeclaration>();
                subDeclarationMap.put(name, subDeclarations);
            }
            return subDeclarations;
        }

        private void addSubToSuper(TypeDeclaration subDeclaration, DeclaredType superType) {
            List<TypeDeclaration> subDeclarations = getSubDeclarationList(superType.getDeclaration());
            subDeclarations.add(subDeclaration);
        }
    }

    private class CollectRenderStubsVisitor extends SimpleDeclarationVisitor {

        public Set<MethodDeclaration> methodDeclarations = new HashSet<MethodDeclaration>();

        @Override
        public void visitInterfaceDeclaration(InterfaceDeclaration interfaceDeclaration) {
            for(MethodDeclaration methodDeclaration: interfaceDeclaration.getMethods()) {
                if (methodDeclaration.getAnnotation(Template.class) != null) {
                    if (MirrorTypes.isString(methodDeclaration.getReturnType())) {
                        if (methodDeclaration.getParameters().size() == 1) {
                            methodDeclarations.add(methodDeclaration);
                        } else {
                            System.out.format("Too many parameters - skipping method\n  %s\n",
                                    formatMethod(interfaceDeclaration, methodDeclaration));
                        }
                    } else {
                        System.out.format("Return type is not string - skipping method\n  %s\n",
                                formatMethod(interfaceDeclaration, methodDeclaration));
                    }
                }
            }
        }

        private String formatMethod(InterfaceDeclaration interfaceDeclaration, MethodDeclaration methodDeclaration) {
            String className = interfaceDeclaration.getSimpleName();
            String methodName = methodDeclaration.getSimpleName();
            String returnTypeName = simpleClassName(methodDeclaration.getReturnType().toString());
            String parametersString = "";
            for (ParameterDeclaration parameterDeclaration: methodDeclaration.getParameters()) {
                String parameterName = parameterDeclaration.getSimpleName();
                String typeName = simpleClassName(parameterDeclaration.getType().toString());
                parametersString += typeName + " " + parameterName + ", ";
            }
            if (! parametersString.isEmpty()) {
                parametersString = parametersString.substring(0, parametersString.length() -2);
            }
            return String.format("%s %s.%s(%s)", returnTypeName, className, methodName, parametersString);
        }

        private String simpleClassName(String qualifiedClassName) {
            String[] tokens = qualifiedClassName.split("\\.");
            if (tokens.length > 0) {
                return tokens[tokens.length -1];
            } else {
                return qualifiedClassName;
            }
        }
    }
}

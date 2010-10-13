package templ.mapper.ast;

public class TemplTypeList extends TemplType {

    private final TemplType elementType;

    public TemplTypeList(TemplType elementType) {
        this.elementType = elementType;
    }

    @Override
    public String toString() {
        return "["+elementType.toString()+"]";
    }

    public TemplType getElementType() {
        return elementType;
    }
}
package templ.mapper.ast;

public class TemplNullableType {
    private final TemplType type;
    private final boolean nullable;

    public TemplNullableType(TemplType type, boolean nullable) {
        this.type = type;
        this.nullable = nullable;
    }

    @Override
    public String toString() {
        return type.toString();
    }

    public TemplType getType() {
        return type;
    }

    public boolean isNullable() {
        return nullable;
    }

    @Override
    public boolean equals(Object o) {
        return o != null
                && o instanceof TemplNullableType
                && this.getType().equals(((TemplNullableType) o).getType())
                && (this.isNullable() == ((TemplNullableType) o).isNullable());
    }
}
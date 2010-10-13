package templ.mapper.ast;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

public class TemplTypeRecord extends TemplType {

    private final Map<String, TemplNullableType> fields;

    public TemplTypeRecord() {
        this.fields = new HashMap<String, TemplNullableType>();
    }

    public TemplTypeRecord(Map<String, TemplNullableType> fields) {
        this.fields = fields;
    }

    public void addField(String name, TemplNullableType nullableType) {
        this.fields.put(name, nullableType);
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("(");
        Iterator<Map.Entry<String, TemplNullableType>> iterator = this.fields.entrySet().iterator();
        while (iterator.hasNext()) {
            Map.Entry<String, TemplNullableType> entry = iterator.next();
            builder.append(entry.getKey());
            builder.append(":");
            builder.append(entry.getValue().toString());
            if (iterator.hasNext()) {
                builder.append(",");
            }
        }
        builder.append(")");
        return builder.toString();
    }

    public Map<String, TemplNullableType> getFields() {
        return fields;
    }
}
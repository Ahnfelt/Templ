package templ.mapper.ast;

import org.junit.Test;

import java.util.HashMap;
import java.util.Map;

import static junit.framework.Assert.*;

public class TemplTypeTest {

    @Test
    public void equalsRecordTest() {
        Map<String, TemplNullableType> es = new HashMap<String, TemplNullableType>();
        es.put("a", new TemplNullableType(new TemplTypeString(), false));
        es.put("b", new TemplNullableType(new TemplTypeList(new TemplTypeString()), false));
        es.put("c", new TemplNullableType(new TemplTypeString(), true));
        TemplType r1 = new TemplTypeRecord(es);

        Map<String, TemplNullableType> es2 = new HashMap<String, TemplNullableType>();
        es2.put("b", new TemplNullableType(new TemplTypeList(new TemplTypeString()), false));
        es2.put("c", new TemplNullableType(new TemplTypeString(), true));
        es2.put("a", new TemplNullableType(new TemplTypeString(), false));
        TemplType r2 = new TemplTypeRecord(es2);
        assertEquals(r1, r2);
    }


    @Test
    public void generalStringsTest() {
        TemplType t1 = new TemplTypeString();
        TemplType t2 = new TemplTypeString();
        System.out.println("t1.general(t2) = " + t1.general(t2));
        assertEquals(t2, t1.general(t2));
    }

    @Test
    public void generalStringListTest() {
        TemplType t1 = new TemplTypeString();
        TemplType t2 = new TemplTypeList(new TemplTypeString());
        System.out.println("t1.general(t2) = " + t1.general(t2));
        assertNull(t1.general(t2));
    }

    @Test
    public void generalStringsRecord() {
        Map<String, TemplNullableType> es = new HashMap<String, TemplNullableType>();
        es.put("a", new TemplNullableType(new TemplTypeString(), false));
        es.put("b", new TemplNullableType(new TemplTypeString(), false));
        es.put("c", new TemplNullableType(new TemplTypeString(), true));
        es.put("d", new TemplNullableType(new TemplTypeString(), true));
        es.put("e", new TemplNullableType(new TemplTypeString(), false));
        es.put("f", new TemplNullableType(new TemplTypeList(new TemplTypeString()), false));
        TemplType r1 = new TemplTypeRecord(es);

        Map<String, TemplNullableType> es2 = new HashMap<String, TemplNullableType>();
        es2.put("b", new TemplNullableType(new TemplTypeString(), false));
        es2.put("c", new TemplNullableType(new TemplTypeString(), true));
        es2.put("d", new TemplNullableType(new TemplTypeString(), false));
        es2.put("e", new TemplNullableType(new TemplTypeString(), true));
        es2.put("f", new TemplNullableType(new TemplTypeString(), false));
        es2.put("g", new TemplNullableType(new TemplTypeString(), false));
        TemplType r2 = new TemplTypeRecord(es2);

        Map<String, TemplNullableType> es3 = new HashMap<String, TemplNullableType>();
        es3.put("b", new TemplNullableType(new TemplTypeString(), false));
        es3.put("c", new TemplNullableType(new TemplTypeString(), true));
        es3.put("d", new TemplNullableType(new TemplTypeString(), true));
        es3.put("e", new TemplNullableType(new TemplTypeString(), true));
        TemplType r3 = new TemplTypeRecord(es3);

        System.out.println("tr.general(r2) = " + r1.general(r2));
        assertEquals(r3, r1.general(r2));
    }
}

package templ.mapper;

import org.junit.Ignore;
import org.junit.Test;
import example.datamodel.ancestor.*;
import example.datamodel.number.AbsInt;

import java.util.Arrays;
import java.util.Collections;

import static junit.framework.Assert.*;

public class ValueMapperTest {

    @Test
    public void convertStringTest() {
        Object v = "banana";
        String expected = "{banana}";
        assertEquals(expected, ValueMapper.convert(v));
    }

    @Test
    public void convertStringEscapeTest() {
        Object v = "$b@a|n{a}na";
        String expected = "{$$b$@a$|n${a$}na}";
        assertEquals(expected, ValueMapper.convert(v));
    }

    @Test
    public void convertEmptyListTest() {
        Object v = Collections.emptyList();
        String expected = "[:()]";
        assertEquals(expected, ValueMapper.convert(v));
    }

    @Test
    public void convertStringListTest() {
        Object v = Arrays.asList("ba{n}ana", "apple");
        String expected = "[{ba${n$}ana},{apple}:()]";
        assertEquals(expected, ValueMapper.convert(v));
    }

    @Test
    public void convertObjectRecordTest() {
        Object v = new Object();
        String expected = "()";
        assertEquals(expected, ValueMapper.convert(v));
    }

    @Test
    public void convertSimpleRecordTest() {
        Object v = new AbsInt(42);
        String expected = "(value={42})";
        assertEquals(expected, ValueMapper.convert(v));
    }

}

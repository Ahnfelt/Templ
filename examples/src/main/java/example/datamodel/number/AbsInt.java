package example.datamodel.number;

public class AbsInt {
    private int value = 0;

    public AbsInt(int value) {
        setValue(value);
    }

    public String getValue() {
        return Integer.toString(value);
    }

    public void setValue(int value) {
        this.value = Math.abs(value);
    }
}

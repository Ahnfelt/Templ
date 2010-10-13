package example.datamodel.number;

public class UnevenInt extends AbsInt {

    public UnevenInt(int value) {
        super(value);
    }

    public void setValue(int value) {
        super.setValue(value%2==1 ? value : value+1);
    }
}
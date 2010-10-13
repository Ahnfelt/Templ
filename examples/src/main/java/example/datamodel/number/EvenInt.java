package example.datamodel.number;

public class EvenInt extends AbsInt {

    public EvenInt(int value) {
        super(value);
    }

    public void setValue(int value) {
        super.setValue(value%2==0 ? value : value+1);
    }
}
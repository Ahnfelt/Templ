package example.datamodel.abc;

public class Y extends X implements I2 {
    private String y = "Yankee";

    public String getY() {
        return y;
    }

    public void setY(String y) {
        this.y = y;
    }

    @Override
    public String getI2() {
        return null;
    }

    @Override
    public String getI1() {
        return null;
    }
}

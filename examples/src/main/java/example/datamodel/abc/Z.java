package example.datamodel.abc;

public class Z extends Y {

    private final String name;
    private final String email;

    public Z(String name) {
        this(name, null);
    }

    public Z(String name, String email) {
        this.name = name;
        this.email = email;
    }

    public String getName() {
        return name;
    }

    public String getEmail() {
        return email;
    }
}
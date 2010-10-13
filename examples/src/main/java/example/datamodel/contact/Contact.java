package example.datamodel.contact;

public class Contact {

    private final String name;
    private final String email;

    public Contact(String name) {
        this(name, null);
    }
    public Contact(String name, String email) {
        this.name = name;
        this.email = email;
    }

    public String getNameX() {
        return name;
    }

    public String getEmail() {
        return email;
    }
}

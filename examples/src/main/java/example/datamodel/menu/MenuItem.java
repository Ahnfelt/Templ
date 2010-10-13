package example.datamodel.menu;


public abstract class MenuItem {
    private String name;

    protected MenuItem(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }

}

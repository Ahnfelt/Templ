package example.datamodel.menu;

import example.datamodel.menu.MenuItem;

import java.util.Collection;

public class SubMenuItem extends MenuItem {
    private Collection<MenuItem> items;

    public SubMenuItem(String name, Collection<MenuItem> items) {
        super(name);
        this.items = items;
    }

    public Collection<MenuItem> getItems() {
        return items;
    }
}

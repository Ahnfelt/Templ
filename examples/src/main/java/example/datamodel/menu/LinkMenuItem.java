package example.datamodel.menu;

import example.datamodel.menu.MenuItem;

public class LinkMenuItem extends MenuItem {
    private String link;

    public LinkMenuItem(String name, String link) {
        super(name);
        this.link = link;
    }

    public String getLink() {
        return link;
    }
}

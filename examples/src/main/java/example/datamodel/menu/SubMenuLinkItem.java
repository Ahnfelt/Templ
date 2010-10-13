package example.datamodel.menu;

import example.datamodel.menu.MenuItem;

import java.util.Collection;

public class SubMenuLinkItem extends SubMenuItem {

    private String link;

    public SubMenuLinkItem(String name, String link, Collection<MenuItem> items) {
        super(name, items);
        this.link = link;
    }

    public String getLink() {
        return link;
    }
}
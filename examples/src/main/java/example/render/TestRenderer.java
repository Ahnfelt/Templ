package example.render;

import example.datamodel.contact.Contact;
import example.datamodel.diamond.A;
import templ.annotation.Template;
import example.datamodel.abc.*;
import example.datamodel.ancestor.*;
import templ.annotation.TemplateRenderers;
import templ.apt.Renderer;
import templ.apt.valuerenderer.*;

@TemplateRenderers({StringRenderer.class, IntegerRenderer.class, DateRenderer.class, CalendarRenderer.class})
public interface TestRenderer extends Renderer {

    @Template("templates/mailto.templ")
    public String renderContact(Contact _);


    // Test String
    /*
    @Template("none.templ")
    public String renderString(String _);
    */

    // Test Lists

    /*
    @Template("templates/none.templ")
    public String renderStringAbstractCollection(AbstractCollection<String> _);

    @Template("templates/none.templ")
    public String renderStringAbstractCollectionAbstractCollection(AbstractCollection<AbstractCollection<String>> _);

    @Template("templates/none.templ")
    public String renderStringCollection(Collection<String> _);

    @Template("templates/none.templ")
    public String renderListX(List<X> _);

    @Template("templates/none.templ")
    public String renderHashSetZ(HashSet<Z> _);
    */

    // Test record inheritance
    @Template("templates/none.templ")
    public String renderX(X _);

    @Template("templates/none.templ")
    public String renderY(Y _);

    @Template("templates/none.templ")
    public String renderZ(Z _);


    // Test record inheritance + nullable

    @Template("templates/bible.templ")
    public String renderAdam(Adam _);

    @Template("templates/bible.templ")
    public String renderCain(Cain _);

    @Template("templates/bible.templ")
    public String renderAbel(Abel _);

    @Template("templates/bible.templ")
    public String renderSeth(Seth _);

    @Template("templates/bible.templ")
    public String renderEnos(Enos _);

    @Template("templates/none.templ")
    public String renderSister(Sister _);
}

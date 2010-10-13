package example.render;

import example.datamodel.contact.Contact;
import templ.annotation.Template;
import example.datamodel.ancestor.*;
import templ.annotation.TemplateRenderers;
import templ.apt.Renderer;
import templ.apt.valuerenderer.*;

@TemplateRenderers({StringRenderer.class, IntegerRenderer.class, DateRenderer.class, CalendarRenderer.class})
public interface TestRenderer extends Renderer {

    @Template("templates/mailto.templ")
    public String renderContact(Contact _);

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

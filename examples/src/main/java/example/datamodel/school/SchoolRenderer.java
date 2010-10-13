package example.datamodel.school;

import templ.annotation.Template;
import templ.apt.Renderer;

public interface SchoolRenderer extends Renderer {
    @Template("templates/school.templ")
    public String renderSchool(PeoplePageModel _);
}
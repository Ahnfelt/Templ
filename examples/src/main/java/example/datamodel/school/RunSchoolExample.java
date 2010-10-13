package example.datamodel.school;

import templ.apt.RendererProxy;

public class RunSchoolExample {

    public static void main(String[] args) {
        Student student1 = new Student("Kasper");
        Student student2 = new Student("Jesper");
        student2.setEmail("email@jesper.dk");
        Teacher teacher = new Teacher("Jonathan", "1234");
        PeoplePageModel pageModel = new PeoplePageModel("DIKU 2009");
        pageModel.addTeacher(teacher);
        pageModel.addStudent(student1);
        pageModel.addStudent(student2);
        String result = RendererProxy.mock(SchoolRenderer.class).renderSchool(pageModel);
        System.out.println(result);
    }
}

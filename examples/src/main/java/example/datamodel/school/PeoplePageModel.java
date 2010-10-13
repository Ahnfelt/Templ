package example.datamodel.school;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

public class PeoplePageModel {
    private final String title;
    private final List<Person> teachers = new LinkedList<Person>();
    private final ArrayList<Person> students = new ArrayList<Person>();

    public PeoplePageModel(String title) {
        this.title = title;
    }

    @NotNull
    public String getTitle() {return title;}

    @NotNull
    public List<Person> getTeachers() {return teachers;}

    @Nullable
    public ArrayList<Person> getStudents() {return students;}

    public void addTeacher(Teacher teacher) {this.teachers.add(teacher);}

    public void addStudent(Student student) {this.students.add(student);}
}

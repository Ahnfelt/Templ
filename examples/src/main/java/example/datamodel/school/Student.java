package example.datamodel.school;

import org.jetbrains.annotations.Nullable;

public class Student extends Person {
    private String email;

    public Student(String name) {super(name);}

    @Nullable
    public String getEmail() {return email;}

    public void setEmail(String email) {this.email = email;}
}

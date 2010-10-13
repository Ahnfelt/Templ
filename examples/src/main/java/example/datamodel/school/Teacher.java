package example.datamodel.school;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

public class Teacher extends Person {
    private final String phone;
    //private String office;

    public Teacher(String name, String phone) {
        super(name); this.phone = phone;
    }

    @NotNull
    public String getPhone() {return phone;}

    //@Nullable
    //public String getOffice() {return office;}

    //public void setOffice(String office) {this.office = office;}
}

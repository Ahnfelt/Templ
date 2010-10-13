package example.datamodel.ancestor;

import org.jetbrains.annotations.NotNull;
import example.datamodel.number.EvenInt;

public class Cain extends Adam {

    @NotNull
    public String getOccupation() {
        return "Farmer";
    }

    public EvenInt getAge() {
        return new EvenInt(910);
    }

}

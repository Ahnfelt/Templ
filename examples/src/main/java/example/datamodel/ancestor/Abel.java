package example.datamodel.ancestor;

import org.jetbrains.annotations.NotNull;
import example.datamodel.number.AbsInt;

public class Abel extends Adam {

    @NotNull
    public String getOccupation() {
        return "Shepherd";
    }

    public AbsInt getAge() {
        return new AbsInt(800);
    }

}
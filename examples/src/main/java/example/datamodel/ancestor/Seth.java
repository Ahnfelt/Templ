package example.datamodel.ancestor;

import org.jetbrains.annotations.Nullable;
import example.datamodel.number.AbsInt;

public class Seth extends Adam {

    @Nullable
    public String getOccupation() {
        return null;
    }

    public AbsInt getAge() {
        return new AbsInt(912);
    }
}
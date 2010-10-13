package example.datamodel.ancestor;

import org.jetbrains.annotations.NotNull;
import example.datamodel.number.UnevenInt;

public class Enos extends Seth {

    @NotNull
    public String getOccupation() {
        return "???";
    }

    public UnevenInt getAge() {
        return new UnevenInt(905);
    }
}
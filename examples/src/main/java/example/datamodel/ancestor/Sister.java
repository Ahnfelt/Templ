package example.datamodel.ancestor;

import org.jetbrains.annotations.NotNull;

import java.util.Arrays;
import java.util.List;

public class Sister extends Eve {

    @Override
    public String getName() {
        return "Unknown Sister";
    }

    @Override
    @NotNull
    public List<Man> getBrothers() {
        return Arrays.asList((Man)new Cain(), new Abel());
    }

}

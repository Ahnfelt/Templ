package example.datamodel.ancestor;

import org.jetbrains.annotations.NotNull;


public class Adam extends Man {

    @Override
    @NotNull
    public String getName() {
        return this.getClass().getSimpleName();
    }
}

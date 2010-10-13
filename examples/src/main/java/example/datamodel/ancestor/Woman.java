package example.datamodel.ancestor;

import org.jetbrains.annotations.Nullable;

import java.util.Collections;
import java.util.List;

public abstract class Woman implements Human {

    @Nullable
    public List<Man> getBrothers() {
        return Collections.emptyList();
    }
}
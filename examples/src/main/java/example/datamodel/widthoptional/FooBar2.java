package example.datamodel.widthoptional;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Arrays;
import java.util.List;

public class FooBar2 {

    @NotNull
    public String getFoo() {
        return "FooBar2: Foo";
    }

    @Nullable
    public List<String> getBar() {
        return Arrays.asList("Foo", "Bar", "Baz");
    }
}
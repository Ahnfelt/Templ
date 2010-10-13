package example.datamodel.widthoptional;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class FooBar {

    @NotNull
    public String getFoo() {
        return "FooBar: Foo";
    }

    @Nullable
    public String getBar() {
        return "FooBar: Bar";
    }
}

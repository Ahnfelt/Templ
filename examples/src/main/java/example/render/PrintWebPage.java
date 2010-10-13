package example.render;

import example.datamodel.abc.Y;
import templ.apt.RendererProxy;

public class PrintWebPage {

    public static void main(String[] args) {
        TestRenderer renderer = RendererProxy.mock(TestRenderer.class);
        Y y = new Y();
        String html = renderer.renderY(y);
        System.out.println(html);
    }
}

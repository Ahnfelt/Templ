package templ.apt;

import org.junit.Ignore;
import org.junit.Test;
import example.datamodel.ancestor.*;
import example.render.TestRenderer;

import static org.junit.Assert.assertTrue;

public class RenderTest {

    @Ignore
    @Test
    public void renderAdamTest() {
        String renderedAdam = RendererProxy.mock(TestRenderer.class).renderAdam(new Adam());
        System.out.println("renderedAdam = " + renderedAdam);

        renderedAdam = RendererProxy.mock(TestRenderer.class).renderAdam(new Cain());
        System.out.println("renderedAdam = " + renderedAdam);

        renderedAdam = RendererProxy.mock(TestRenderer.class).renderAdam(new Abel());
        System.out.println("renderedAdam = " + renderedAdam);

        renderedAdam = RendererProxy.mock(TestRenderer.class).renderAdam(new Seth());
        System.out.println("renderedAdam = " + renderedAdam);

        renderedAdam = RendererProxy.mock(TestRenderer.class).renderAdam(new Enos());
        System.out.println("renderedAdam = " + renderedAdam);

        assertTrue(true);
    }
}

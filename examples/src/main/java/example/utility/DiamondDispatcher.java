package example.utility;

import example.datamodel.diamond.A;
import templ.utility.Dispatcher;
import templ.utility.DispatcherProxy;
import templ.utility.Dispatcher2;
import example.datamodel.diamond.C;
import example.datamodel.diamond.B;
import example.datamodel.diamond.D;

public class DiamondDispatcher {

    public static class _A implements Dispatcher<A, String> {
        public String dispatch(A value) {
            return "A";
        }
    }

    public static class _ABC extends _A {
        public String dispatch(B value) {
            return "B";
        }

        public String dispatch(C value) {
            return "C";
        }
    }

    public static class _ABCD extends _ABC {
        public String dispatch(D value) {
            return "D";
        }
    }

    public static class Fruit {}
    public static class Apple extends Fruit {}

    public static class AppleDispatcher1 implements Dispatcher2<Fruit, Fruit, String> {
        public String dispatch(Fruit x, Fruit y) {
            return "Fruit, Fruit";
        }
    }

    public static class AppleDispatcher2 extends AppleDispatcher1 {
        public String dispatch(Apple x, Fruit y) {
            return "Apple, Fruit";
        }

        public String dispatch(Fruit x, Apple y) {
            return "Fruit, Apple";
        }
    }

    public static class AppleDispatcher3 extends AppleDispatcher1 {
        public String dispatch(Fruit x, Apple y) {
            return "Apple, Apple";
        }
    }

    public static void main(String args[]) {
        Dispatcher<A, String> a = DispatcherProxy.mock(new _A());
        Dispatcher<A, String> abc = DispatcherProxy.mock(new _ABC());
        Dispatcher<A, String> abcd = DispatcherProxy.mock(new _ABCD());
        System.out.println("Diamond D < B,C < A. ");
        System.out.println("_A.dispatch(D): " + a.dispatch(new D()));
        System.out.println("_ABC.dispatch(D): " + abc.dispatch(new D()));
        System.out.println("_ABCD.dispatch(D): " + abcd.dispatch(new D()));
        System.out.println("Two arguments:");
        Dispatcher2<Fruit, Fruit, String> d1 = DispatcherProxy.mock(new AppleDispatcher1());
        Dispatcher2<Fruit, Fruit, String> d2 = DispatcherProxy.mock(new AppleDispatcher2());
        Dispatcher2<Fruit, Fruit, String> d3 = DispatcherProxy.mock(new AppleDispatcher3());
        System.out.println("F.dispatch(Apple, Apple): " + d1.dispatch(new Apple(), new Apple()));
        System.out.println("FA.dispatch(Apple, Apple): " + d2.dispatch(new Apple(), new Apple()));
        System.out.println("AA.dispatch(Apple, Apple): " + d3.dispatch(new Apple(), new Apple()));
        String r = DispatcherProxy.match(new Apple(), new Apple(), new Dispatcher2<Fruit, Fruit, String>() {
            public String dispatch(Fruit v1, Fruit v2) {
                return "Bad mojo";
            }
            public String dispatch(Fruit v1, Apple v2) {
                return "Fruity";
            }
        });
        System.out.println("Match: " + r);
    }
}

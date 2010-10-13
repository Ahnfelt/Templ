package templ.utility;

// TODO: Consider picking the most specific common subtype when faced with a diamond

import java.lang.reflect.Proxy;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;

public class DispatcherProxy implements InvocationHandler {
    private final Object dispatcher;

    @SuppressWarnings("unchecked")
    public static <I extends Dispatcher<T, R>, T, V extends T, R> R match(
        V value, I dispatcher) {
        return ((Dispatcher<T, R>) DispatcherProxy.mock(dispatcher, Dispatcher.class)).dispatch(value);
    }

    @SuppressWarnings("unchecked")
    public static <I extends Dispatcher2<T1, T2, R>, T1, T2, V1 extends T1, V2 extends T2, R> R match(
        V1 value1, V2 value2, I dispatcher) {
        return ((Dispatcher2<T1, T2, R>) DispatcherProxy.mock(dispatcher, Dispatcher2.class)).dispatch(value1, value2);
    }

    @SuppressWarnings("unchecked")
    public static <I extends Dispatcher<? extends T, R>, T, R> Dispatcher<T, R> mock(I dispatcher) {
        return (Dispatcher<T, R>) DispatcherProxy.mock(dispatcher, Dispatcher.class);
    }

    @SuppressWarnings("unchecked")
    public static <I extends Dispatcher2<T1, T2, R>, T1, T2, R> Dispatcher2<T1, T2, R> mock(I dispatcher) {
        return (Dispatcher2<T1, T2, R>) DispatcherProxy.mock(dispatcher, Dispatcher2.class);
    }

    @SuppressWarnings("unchecked")
    public static Object mock(Object dispatcher, Class dispatcherClass) {
        return Proxy.newProxyInstance(
	        dispatcher.getClass().getClassLoader(),
	        new Class[] { dispatcherClass },
            new DispatcherProxy(dispatcher));
    }

    private DispatcherProxy(Object dispatcher) {
        this.dispatcher = dispatcher;
    }

    public Object invoke(Object object, Method method, Object[] objects) throws Throwable {
        if(method.getName().equals("dispatch")) {
            Class[] classes = new Class[objects.length];
            for(int i = 0; i < objects.length; i++) {
                classes[i] = objects[i].getClass();
            }
            Class[] parameters = method.getParameterTypes();
            for(Method newMethod: dispatcher.getClass().getMethods()) {
                Class[] newParameters = newMethod.getParameterTypes();
                if(newMethod.getName().equals("dispatch") && newParameters.length == parameters.length) {
                    boolean better = true;
                    for(int i = 0; i < objects.length; i++) {
                        if(!newParameters[i].isAssignableFrom(classes[i])) {
                            better = false;
                            break;
                        }
                    }
                    if(better) {
                        for(int i = 0; i < objects.length; i++) {
                            if(!parameters[i].isAssignableFrom(newParameters[i])) break;
                            if(!newParameters[i].isAssignableFrom(parameters[i])) {
                                method = newMethod;
                                parameters = newParameters;
                                break;
                            }
                        }
                    }
                }
            }
            // Sometimes it is convenient to use this on private inner classes
            method.setAccessible(true);
            return method.invoke(dispatcher, objects);
        } else {
            return method.invoke(object, objects);
        }
    }
}

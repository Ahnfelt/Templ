package templ.mapper.ast;

import templ.utility.Dispatcher;
import templ.utility.Dispatcher2;
import templ.utility.DispatcherProxy;

import java.util.Set;

public abstract class TemplType {

    public TemplType general(TemplType t) {
        return DispatcherProxy.mock(new Generalizer()).dispatch(this, t);
    }

    @Override
    public boolean equals(Object o) {
        return o != null && DispatcherProxy.mock(new Equalizer()).dispatch(this, o);
    }

    public static class Equalizer implements Dispatcher2<Object, Object, Boolean> {
        public Boolean dispatch(Object t1, Object t2) {
            return false;
        }

        public Boolean dispatch(TemplTypeString t1, TemplTypeString t2) {
            return true;
        }

        public Boolean dispatch(TemplTypeList t1, TemplTypeList t2) {
            return t1.getElementType().equals(t2.getElementType());
        }

        public Boolean dispatch(TemplTypeRecord t1, TemplTypeRecord t2) {
            return t1.getFields().equals(t2.getFields());
        }
    }

    public static class Generalizer implements Dispatcher2<TemplType, TemplType, TemplType> {
        public TemplType dispatch(TemplType t1, TemplType t2) {
            return null;
        }

        public TemplType dispatch(TemplTypeString t1, TemplTypeString t2) {
            return t1;
        }

        public TemplType dispatch(TemplTypeList t1, TemplTypeList t2) {
            TemplType t = t1.getElementType().general(t2.getElementType());
            if (t == null) return null;
            else return new TemplTypeList(t);
        }

        public TemplTypeRecord dispatch(TemplTypeRecord t1, TemplTypeRecord t2) {
            Set<String> fs1 = t1.getFields().keySet();
            Set<String> fs2 = t2.getFields().keySet();
            fs1.retainAll(fs2); // intersection
            TemplTypeRecord rt = new TemplTypeRecord();
            for (String fieldName: fs1) {
                TemplNullableType nt1 = t1.getFields().get(fieldName);
                TemplNullableType nt2 = t2.getFields().get(fieldName);
                TemplType t = nt1.getType().general(nt2.getType());
                boolean nullable = nt1.isNullable() || nt2.isNullable();
                if (t != null) {
                    rt.addField(fieldName, new TemplNullableType(t, nullable));
                }
            }
            return rt;
        }
    }
}

package it.denzosoft.jprolog.builtin;

import it.denzosoft.jprolog.Prolog;
import it.denzosoft.jprolog.QuerySolver;
import java.lang.reflect.Field;

public class BuiltInHelper {

    //This is fragile. Consider refactoring BuiltIn interface if possible.
    public static Prolog getPrologInstance(QuerySolver solver) {
        Field[] fields = solver.getClass().getDeclaredFields();
        for (Field field : fields) {
            if (Prolog.class.isAssignableFrom(field.getType())) {
                try {
                    field.setAccessible(true); // Access private fields if necessary (fragile!)
                    Object value = field.get(solver);
                    if (value instanceof Prolog) {
                        return (Prolog) value;
                    }
                } catch (IllegalAccessException e) {
                    // Ignore or log
                }
            }
        }
        return null; // Not found
    }
}

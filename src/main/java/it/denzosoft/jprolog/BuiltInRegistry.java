package it.denzosoft.jprolog;

import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

public class BuiltInRegistry {
    private static final Logger LOGGER = Logger.getLogger(BuiltInRegistry.class.getName());
    private final Map<String, BuiltIn> builtIns = new HashMap<>();

    /**
     * Register a built-in predicate.
     * 
     * @param name The predicate name
     * @param builtIn The built-in implementation
     */
    public void registerBuiltIn(String name, BuiltIn builtIn) {
        if (builtIns.containsKey(name)) {
            LOGGER.warning("Overriding existing built-in predicate: " + name);
        }
        builtIns.put(name, builtIn);
    }

    /**
     * Get a built-in predicate by name.
     * 
     * @param name The predicate name
     * @return The built-in implementation or null if not found
     */
    public BuiltIn getBuiltIn(String name) {
        return builtIns.get(name);
    }

    /**
     * Check if a built-in predicate exists.
     * 
     * @param name The predicate name
     * @return true if the built-in exists
     */
    public boolean hasBuiltIn(String name) {
        return builtIns.containsKey(name);
    }
}

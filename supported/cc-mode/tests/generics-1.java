
public class generics {

    Map<String, Driver> allDrivers = new Map<String, Driver>();

    static <T> void fromArrayToCollection(T[] a, Collection<T> c) {
        for (T o: a) {
            c.add(o);
        }
    }

}

interface Collection<E> {
    public <T> boolean containsAll(Collection<T> c);
    public <T extends E> boolean addAll(Collection<T> c);
}

public class GenericsTest<T extends Comparable> {
    static List<List<? extends Shape>> history =
	new List<List<? extends Shape>>();

    public void drawAll(List<? extends Shape> shapes) {
        history.addLast(shapes);
        for (Shape s: shapes) {
            s.draw(this);
        }
    }

    public static <T> T writeAll(Collection<T> coll, Sink<? super T> snk){}
    public static <T extends Comparable<T>>
        T max(Collection<T> coll) {
    }

    public static <T extends Object & Comparable<? super T>>
        ArrayList max(Collection<T> coll) {
        T a;
        T b;
    }

    public static <T extends Object & Comparable<? super T>>
        T max(Collection<? extends T> coll) {
    }

    public <T> void GenericsTest {
        ArrayList<ArrayList<Integer>> a = new ArrayList<ArrayList<Integer>>();

        HashMap<String, Integer> b = new HashMap<String, Integer>;
        ArrayList<?> = new ArrayList<?>;
        int foo;
    }

}

public interface List<E> {
    void add(E x);
    public Iterator iterator();
}

public class Census {
    public static void
        addRegistry(Map<String, ? extends Person> registry) {
    }

    public void drawAll( List<? extends Shape> shapes) {
    }

    public <T> void GenericsTest {
    }

    static <T> void fromArrayToCollection(T[] a, Collection<T> c) {
        for (T o: a) {
            c.add(o);
        }
    }

    Map<String, Driver> allDrivers = new Map<String, Driver>();
}

@NonNull
@TestClass
@FooBar public class Annotations {

}

@NonNull public class AnotherTest {
    @NonNull int var1;
    @Negative
    int var2;

    @NonNegative
    @NonNull int var2;
}

@NonNull
public class AnnotationTest {

    @NonNull int expected;

    @Test
    public void foo() throws Exception {
        int a;
        @NonNull
	    int a;
        @NonNull ArrayList a;
        @NonNull
	    @NonNegative int a;
        @NonNull @NonNegative ArrayList a;
    }

    @Foo
    public void bar( @NonNull int Bla,
		     @NonNegative @NonNull int test ) {
    }

    @Foo("hi")
    public void test() {

    }

}

@Author(name = "Benjamin Franklin", date = "3/27/2003")
public class MyClass() { }

public @interface RequestForEnhancement {
    int pvalue();
    int lvalue();
    int value();
}

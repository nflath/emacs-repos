class Foo {

    void bar (int i, int j) {
	int k;
    }

    void bar (int i, int j) throws IOException {
	int k;
    }
    void bar (int i, int j) {
	int k;
    }
    void baz (int i,
	      int j) {
	int k;
    }
    void bad1 (int i,
	       int j)
	throws IOException {
	int k;
    }
    void bad2 (int i,
	       int j) throws IOException {
	int k;
    }
}

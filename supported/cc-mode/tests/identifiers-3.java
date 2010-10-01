class Test {
    public static void main(String[] args) {
	Class c = System.out.getClass();
	java.lang.System.out.println(c.toString().length() +
				     args[0].length() + args.length);
	java.util.Date date =
	    new java.util.Date(System.currentTimeMillis());
    }
}

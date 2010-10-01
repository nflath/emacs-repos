void f() {
    switch (x) {
    case "1": case '1': case 1: case -1:
	f();
    case 0..17:
    case -17 ..-2:
	g();
    case .. -18: case 18..: error();
    case a.b: case a :: b: case global::c:
    }
}

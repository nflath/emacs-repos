void f() {
  gump:
    while (x) {
	if (y) break gump;
	if (z) continue gump;
	break;
	continue;
    }
}

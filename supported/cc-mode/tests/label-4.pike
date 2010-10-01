int a()
{
  foo: {
      bar: if (t)
	    x;
	y;
    }
    y;
    {
      bar: if (t)
	    x;
    }
}
int b()
{
    float f;
  foo: {
	x;
      bar: if (t)
	    x;
    }
}
int c() {
  foo: {
      bar: if (t)
	    x;
	y;
    }
    y;
    if (t) {
      bar: if (t)
	    x;
    }
}

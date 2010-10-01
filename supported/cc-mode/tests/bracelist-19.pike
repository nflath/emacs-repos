int main()
{
    foo (a, ({
	     b, c,
	     d,
	     ({e})
	 }));
    foo (a, ({
	     ({e}),
	     d
	 }));
}

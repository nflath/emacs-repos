class A
{
    int x;
private:
    class B
    {
    private:
	int x;
    };
    class C
    {
	int x;
    private:
    };
};
class C {
    int x;
private:
    class B {
    private:
	int x;
    };
    class C {
	int x;
    private:
    }
}

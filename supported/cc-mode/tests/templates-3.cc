class A: public B {
public:
    A();
};

class A: public B<int> {
public:
    A();
};

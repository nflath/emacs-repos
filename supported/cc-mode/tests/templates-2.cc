template <class T>
class Foo : T
{
public:
    Foo( void );
    ~Foo( void );
};


template <class T, class S>
class Foo : private T, public S
{
public:
    Foo( void );
    ~Foo( void );
};

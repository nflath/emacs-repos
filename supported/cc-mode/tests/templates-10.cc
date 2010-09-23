template <Polarization P>
struct AtomTerm
{
    virtual ~AtomTerm();
    virtual AtomComp<P>* get_atom() const = 0;
};

class EpeeValue
{
    int  status;
    int  date;
    char *user;
    char *method;
    EpeeType *type;
    
    union
    {   IntRange irange;
        RealRange rrange;
        char *string;
        EpeeOID oid;
        EpeeList list;
        EpeeArray array;
    };

}

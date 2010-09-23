NoWork::NoWork(const string& name,
               int index) :
    name_(name),
    index_(index)
{
}

Work::Work(const string& name) :
    name_(name) 
{
}

WorkAlso::WorkAlso(const string& name,
                   const string& index) :
    name_(name),
    index_(index)
{
}

AlsoOk::AlsoOk(int           index,
               const string& name) :
    index_(index),
    name_(name)   
{
}

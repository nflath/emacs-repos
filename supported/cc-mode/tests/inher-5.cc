class Foo
    : public Bar,
      private Gnu <int,
		   string>
{};
class Foo <int, string>
    : public Bar,
      private Gnu <int,
		   string>
{};
class Foo <int,
	   string>
    : public Bar,
      private Gnu <int,
		   string>
{};
class Foo <int, string>
    : public Bar <int>,
      private Gnu <int,
		   string>
{};
class Foo <int,
	   string>
    : public Bar <int,
		  string>,
      private Gnu <int,
		   string>
{};

class Foo :
    public Bar,
    private Gnu <int,
		 string>
{};
class Foo <int, string> :
    public Bar,
    private Gnu <int,
		 string>
{};
class Foo <int,
	   string> :
    public Bar,
    private Gnu <int,
		 string>
{};
class Foo <int, string> :
    public Bar <int>,
    private Gnu <int,
		 string>
{};
class Foo <int,
	   string> :
    public Bar <int,
		string>,
    private Gnu <int,
		 string>
{};

class Foo : public Bar,
	    private Gnu <int,
			 string>
{};
class Foo <int, string> : public Bar,
			  private Gnu <int,
				       string>
{};
class Foo <int,
	   string> : public Bar,
		     private Gnu <int,
				  string>
{};
class Foo <int, string> : public Bar <int>,
			  private Gnu <int,
				       string>
{};
class Foo <int,
	   string> : public Bar <int,
				 string>,
		     private Gnu <int,
				  string>
{};

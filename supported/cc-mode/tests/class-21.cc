struct STR {
    int i;
    struct STR2 {float f; char *s;}
}
    str_1 = /* ; */ {1,
		     {1.0,
		      "foo"
		     }},
    str_2 = {2, {-1.0, "bar"}};

class CLS {
    int i; float f; char *s;
}
    cls_1 /* ; */ (1,
		   1.0,
		   "foo"
	),
    cls_2 (2, -1.0, "bar");

struct STR {
    int i; float f;
} str_1 = /* ; */ {1,
		   1.7
},
    str_2 = {2, 3.1};

struct STR {
    int i; float f;
} str_1 = /* ; */ {1, 1.7},
    str_2 = {2,
	     3.1
    };

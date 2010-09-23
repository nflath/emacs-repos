int `== (int a, int b)
{
    return !(a != b);
}
mapping(string:array(array)) table_init =
([
    "languages": ({"Danish", "English",
		   "Finnish", "Norwegian", "Swedish"
		   ]);

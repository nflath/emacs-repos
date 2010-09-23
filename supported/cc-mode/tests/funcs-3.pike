array(string)
drop_first_element(array(string) elements)
{
    return elements[1..];
}

int
main(int argc, array(string) argv)
{
    write(drop_first_element(argv) * "\n" + "\n");
}

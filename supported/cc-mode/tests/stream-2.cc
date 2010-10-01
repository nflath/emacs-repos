int main()
{
    out_stream <<
        // Determine if the device prints in color or in black and white.
        "%% Determine if the device prints in color or in black and white.\n"
        "/BlackAndWhiteDevice\n"
        "    statusdict begin\n"
        "        /processcolors where\n"
        "        { pop processcolors } { 1 } ifelse\n"
        "    end\n"
        "    1 eq\n"
        "def\n\n";
}

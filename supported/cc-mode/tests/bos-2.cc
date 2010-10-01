main() {
    out_stream
        << "%% Function that takes an azimuth and deviation to (px,py).\n"
        << "%% usage: azimuth deviation AziDevToPxPy.\n"
        << "/AziDevToPxPy {\n"
        << "    /dev exch def\n"
        << "    /azi exch def\n"
        << "    /rad dev DevToRadius def\n"
        << "    /px rad azi sin mul def\n"
        << "    /py rad azi cos mul def\n"
        << "    px py\n"
        << "} def\n\n"
        <<;
}

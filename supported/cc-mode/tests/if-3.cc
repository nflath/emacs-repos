int main()
{
    if (
        (( slope >= 0.0 ) && ( slope >= Deriv(j) )) ||
        (( slope <= 0.0 ) && ( slope <= Deriv(j) ))
        )
        Deriv(0) = 2.0 * slope - Deriv(1);
    else
        Deriv(0) = slope + (Abs(slope) * (slope - Deriv(1))) /
            ( Abs(slope) + Abs(slope - Deriv(1)) );
}

int main()
{
    switch(PEXUtCheckColorApproximation(dpy, &(capx[i]), vis_info,
					&alt_capx)) {
    case PEXUtSuccess:
    case PEXUtQualifiedSuccess:
        pex_capx = &capx[i];
        found = TRUE;
        break;
    case PEXUtAlternativeSuccess:
        pex_capx = &alt_capx;
        found = TRUE;
    } /* switch */
}

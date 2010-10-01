int main()
{
    if ( column_tags.IXYZborehole_azimuth < 0 ) {
        if ( column_tags.IXYZpad_1_azimuth > -1 &&
             column_tags.IIJKpad_1_angle > -1 &&
             column_tags.Ideviation > -1 ) {
            loop_again = 1;
            outfile += "A";
            column_tags.IXYZborehole_azimuth = number_columns;
            vXYZborehole_azimuth.reference(data.col(number_columns));
            ++number_columns;
            for (int i=0; i<rows; i++)
                vXYZborehole_azimuth(i) =
                    Mod(vXYZpad_1_azimuth(i) -
                        atan2f(cosf(vIJKpad_1_angle(i)),
                               sinf(vIJKpad_1_angle(i)) * cosf(vdeviation(i))
                            ),
                        2.0 * BZ_PI);
            if ( c_verbose )
                cout << "Generating A from DpP." << endl;
        }
        else if ( column_tags.IXYZpad_1_azimuth > -1 &&
                  column_tags.IXYZrelative_bearing > -1 ) {
            loop_again = 1;
            outfile += "A";
            column_tags.IXYZborehole_azimuth = number_columns;
            vXYZborehole_azimuth.reference(data.col(number_columns));

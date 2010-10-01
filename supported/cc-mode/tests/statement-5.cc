int main()
{
    int j;
    for (j=0; j<number_columns; j++)
        if ( j != column_tags.Idepth )
            row(0, j) = mean((data.col(j))(range));
        else
            row(0, j) = (zone_start_depth + vdepth(IJK_zone_end_pos))/2.0;

    column_tags.wrapData(row);
}

enum infoE                                         
TypeOfThing(grobjp node)
{
    if (strcmp(obj_label(node), "$Database") == 0)
        return(DATABASEi);
    if (strcmp(obj_label(node), "$Program") == 0) {
        return(PROGRAMi);
    }
    return(PATTERNi);
}

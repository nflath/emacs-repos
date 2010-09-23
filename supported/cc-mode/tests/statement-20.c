void x()
{
    if (sp[-1].type == T_MAPPING)
	if (sp[-1].u.mapping->md) {
	    foo();
	}
}
void y()
{
    if (sp[-1].type == T_MAPPING)
	NEW_MAPPING_LOOP (sp[-1].u.mapping->md) {
	    foo();
	}
}
void z()
{
    if (sp[-1].type == T_MAPPING)
	NEW_MAPPING_LOOP (sp[-1].u.mapping->md)
	    {
		foo();
	    }
}

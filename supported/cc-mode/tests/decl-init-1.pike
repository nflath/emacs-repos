RXML.Type content_type =
    (float) my_configuration()->query ("compat_level") >= 3.4 ?
    RXML.t_text(RXML.PXml) :
    RXML.t_xml(RXML.PXml);	// Not a member-init-intro.

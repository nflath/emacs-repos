// Some real-world Objective-C for testing method definitions.  This
// example is a mutilated String.m from the libobjects GNU project.

#include <objects/String.h>

@implementation String

#if HAVE_VSPRINTF
- initWithFormat: (id <String>)aFormatString arguments: (va_list)arg
{
    char buf[128];		/* xxx horrible, disgusting, fix this */
    vsprintf(buf, [aFormatString cString], arg);
    return [self initWithCString:buf];
}
#endif /* HAVE_VSPRINTF */

- initWithFormat: (id <String>)aFormatString, ...
{
    va_list ap;
    va_start(ap, aFormatString);
    [self initWithCFormat:[aFormatString cString] arguments:ap];
    va_end(ap);
    return self;
}

- init
{
    return [self initWithCString:""];
}

+ stringWithString: (id <String>)aString range: (IndexRange)aRange
{
    return [[[CString alloc] initWithString:aString range:aRange]
	       autorelease];
}

+ stringWithCString: (const char*)cp range: (IndexRange)r
{
    return [[[CString alloc] initWithCString:cp range:r]
	       autorelease];
}

- (char *) cStringCopyRange: (IndexRange)aRange;
{
    [self notImplemented:_cmd];
    return "";
}

- copyWithZone: (NSZone*)z
{
    return [[[self class] allocWithZone:z] initWithString:(NSString*)self];
}

// Note incorrect syntax on the following line.
- (int(*)(elt,elt)) comparisonFunction
{
    return elt_compare_chars;
}

- (t1) gazonk: (t2) u, t3 x, int y, ...
{
}

@end

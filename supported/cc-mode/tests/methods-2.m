@implementation IndentTest : NSObject
- (void) test
{
    int a = 0;
    int b = 0;
    int c = 0;

    if (a == b)
	{
	    a = b
		+ c;
	}
}
@end

+ (void) initialize
{
    if (self == [NSScrollView class])
	{
	    NSDebugLog (@"Initialize NSScrollView class\n");
	    [self setVersion: 1];
	}
}

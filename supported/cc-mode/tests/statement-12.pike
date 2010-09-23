int main()
{
    if (!fn)
	fn = this->do_enter ?
	    [int|function(RequestID:int|function)] this->do_enter (ctx->id) :
	    1;
    if (!fn)
	fn = this->do_enter ?
	    ([this->do_enter (ctx->id)]) :
	    1;
}

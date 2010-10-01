void f() {
    //*
    x;
}
// A bug in forward-comment in (at least) XEmacs 21.4 causes the line
// "x;" above to be analyzed as ((defun-block-intro 1) (statement-cont
// 1)).  (forward-comment -1) at the beginning of that line should go
// to the first "/" in the line above and not to the position after "*".
//
// Local Variables:
// cc-test-skip: (xemacs-21)
// End:

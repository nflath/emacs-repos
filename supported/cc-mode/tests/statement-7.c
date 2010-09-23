int main() 
{
    XtAddCallback(okButton,XmNactivateCallback,
                  (XtCallbackProc)closeProductDescriptionCallback,
                  (XtPointer)productDescriptionShell); n++;
    XmStringFree(okXmString);

    XtSetArg(args[n],XmNmwmDecorations, MWM_DECOR_ALL|
             MWM_DECOR_BORDER|MWM_DECOR_RESIZEH|MWM_DECOR_TITLE|MWM_DECOR_MENU|
             MWM_DECOR_MINIMIZE|MWM_DECOR_MAXIMIZE); n++;
    XtSetArg(args[n],XmNtitle,"Version"); n++;
}

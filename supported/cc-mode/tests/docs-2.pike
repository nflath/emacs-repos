//! @decl RxNode test (function(DataList,void|Rx.Rx.Process:int) func, @
//!		       void|int low, void|int high)
//! FIXME: Multiline declarations aren't fontified correctly.

#define DataList mixed
//! @decl typedef mixed DataList;
//!
//! Denotes the type of a list of @[Data] values.
//!
//! @seealso
//! @[SymbolHandler.empty_data_list] and
//! @[SymbolHandler.append_value].

#define LaxRxType object(RxNode)|string|array|multiset|mapping
//! @decl typedef RxNode|string|array(LaxRxType)|@
//!               multiset(LaxRxType)|@
//!               mapping(LaxRxType:LaxRxType) LaxRxType
//! FIXME: Multiline declarations aren't fontified correctly.

class X {
    mapping(int|string:mixed) submatches (DataList input, void|mixed extra);
    //! Returns the submatches found in the string @[input].
    // //! A mapping is returned if there are any submatches with string
    // //! names in the regexp, otherwise an array is used. If an array is
    // //! returned, it's always long enough to hold all submatches, and
    // //! it contains zero in all elements that didn't match anything.

    //! @[foo
    //! ]
    //! xyz @seealso
    //! @tt{({outgoing_targets (state), outgoing_ulabels (state),
    //! outgoing_llabels (state)})@}.

    //! @variable a_very_long_@
    //!variable_name
    //!but this is not part of it.
    //! @seealso
    //! bar

    mapping misc = ([]);
    //! @mapping
    //! @member Rx "rx"
    //!   The @[Rx] object this FSA is built in.
    //! @member multiset(State) "nonaccessible"
    //!   States which might not be accessible, i.e. to which there
    //!   might not be a path from the start state.
    //! @endmapping
}

//! @decl void create("stdin")
//! @decl void create("stdin", function accept_callback)
//!
//! If the first argument is other than @tt{"stdin"@} the arguments will
//! be passed to @[bind()].

/*! @class Ansgar */

/*! @endclass Ansgar */


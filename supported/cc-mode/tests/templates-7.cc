template <
    class InputIterator,
    class CompareFuncObj> class Foo {};
typedef MBSLockedPtr<MSString,
		     RWReadersWriterLock<MSString,
					 WriteLock>,
		     RWReadersWriterLock::WriteLockGuard> WriteVal;

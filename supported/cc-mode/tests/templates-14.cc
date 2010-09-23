template <class T, size_t ACE_SIZE> ACE_INLINE int
ACE_Fixed_Set<T, ACE_SIZE>::is_empty (void) const
{
  return this->cur_size_ == 0;
}

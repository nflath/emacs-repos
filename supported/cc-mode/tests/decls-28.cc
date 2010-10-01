void f() {
    ACE_Time_Value tv (requested->tv_sec,
		       requested->tv_nsec / 1000);
    return ACE_OS::sleep (tv);
}

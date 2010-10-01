template <class _EQWLaser, oss::Polarization P, class _Grating>
inline
QWLaserBase<_EQWLaser, P, _Grating>::
QWLaserBase(double time_step,
	    Contact<P, n>* contact,
	    _Grating* grating = 0,
	    const char* name = 0)
    : base(_EQWLaser(name,
		     time_step,
		     contact,
		     grating ? grating->get_osigrating() : 0),
	   name)
{
}

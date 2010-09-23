public class Ed
{
    public RidesOn[] getRidesAsDependent (String label)
    {
	String query = "select rides_on_id from dp_rides_on where depending_id="
		       + productID
		       + " and rides_on_status='" + ridesOnStatus
		       + "' and riding_type = '" + label + "'";
    }
}
// Local Variables:
// c-file-offsets: ((statement-cont . c-lineup-math))
// End:

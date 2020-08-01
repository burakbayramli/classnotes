package my.project.MyCamera;

import java.io.Serializable;

public class GeoPoint implements MathConstants, GeoConstants {

    static final long serialVersionUID = 1L;

    private double mLongitude;
    private double mLatitude;
    private double mAltitude;
    
    public GeoPoint(final double aLatitude, final double aLongitude) {
	this.mLatitude = aLatitude;
	this.mLongitude = aLongitude;
    }

    public GeoPoint(final double aLatitude, final double aLongitude, final double aAltitude) {
	this.mLatitude = aLatitude;
	this.mLongitude = aLongitude;
	this.mAltitude = aAltitude;
    }

    public GeoPoint(final GeoPoint aGeopoint) {
	this.mLatitude = aGeopoint.mLatitude;
	this.mLongitude = aGeopoint.mLongitude;
	this.mAltitude = aGeopoint.mAltitude;
    }

    public double getLongitude() {
	return this.mLongitude;
    }

    public double getLatitude() {
	return this.mLatitude;
    }

    public double getAltitude() {
	return this.mAltitude;
    }

    public void setLatitude(final double aLatitude) { this.mLatitude = aLatitude; }

    public void setLongitude(final double aLongitude) { this.mLongitude = aLongitude; }

    public void setAltitude(final double aAltitude) {
	this.mAltitude = aAltitude;
    }

    public void setCoords(final double aLatitude, final double aLongitude) {
	this.mLatitude = aLatitude;
	this.mLongitude = aLongitude;
    }

    @Override
    public GeoPoint clone() {
	return new GeoPoint(this.mLatitude, this.mLongitude, this.mAltitude);
    }
     
    public String toIntString() {
	return new StringBuilder().
	    append(((int)(this.mLatitude*1E6))).
	    append(",").
	    append(((int)(this.mLongitude*1E6))).
	    append(",").
	    append((int)(this.mAltitude))
	    .toString();
    }

    @Override
    public String toString() {
	return new StringBuilder().append(this.mLatitude).append(",").append(this.mLongitude).append(",").append(this.mAltitude)
	    .toString();
    }

    @Override
    public boolean equals(final Object obj) {
	if (obj == null) {
	    return false;
	}
	if (obj == this) {
	    return true;
	}
	if (obj.getClass() != getClass()) {
	    return false;
	}
	final GeoPoint rhs = (GeoPoint) obj;
	return rhs.mLatitude == this.mLatitude && rhs.mLongitude == this.mLongitude && rhs.mAltitude == this.mAltitude;
    }

    @Override
    public int hashCode() {
	return 37 * (17 * (int)(mLatitude*1E-6) + (int)(mLongitude*1E-6)) + (int)mAltitude;
    }

    public int distanceTo(final GeoPoint other) {

	final double a1 = DEG2RAD * this.mLatitude;
	final double a2 = DEG2RAD * this.mLongitude;
	final double b1 = DEG2RAD * other.getLatitude();
	final double b2 = DEG2RAD * other.getLongitude();

	final double cosa1 = Math.cos(a1);
	final double cosb1 = Math.cos(b1);

	final double t1 = cosa1 * Math.cos(a2) * cosb1 * Math.cos(b2);

	final double t2 = cosa1 * Math.sin(a2) * cosb1 * Math.sin(b2);

	final double t3 = Math.sin(a1) * Math.sin(b1);

	final double tt = Math.acos(t1 + t2 + t3);

	return (int) (RADIUS_EARTH_METERS * tt);
    }

    public double bearingTo(final GeoPoint other) {
	final double lat1 = Math.toRadians(this.mLatitude);
	final double long1 = Math.toRadians(this.mLongitude);
	final double lat2 = Math.toRadians(other.getLatitude());
	final double long2 = Math.toRadians(other.getLongitude());
	final double delta_long = long2 - long1;
	final double a = Math.sin(delta_long) * Math.cos(lat2);
	final double b = Math.cos(lat1) * Math.sin(lat2) -
	    Math.sin(lat1) * Math.cos(lat2) * Math.cos(delta_long);
	final double bearing = Math.toDegrees(Math.atan2(a, b));
	final double bearing_normalized = (bearing + 360) % 360;
	return bearing_normalized;
    }

    public GeoPoint destinationPoint(final double aDistanceInMeters, final float aBearingInDegrees) {

	// convert distance to angular distance
	final double dist = aDistanceInMeters / RADIUS_EARTH_METERS;

	// convert bearing to radians
	final float brng = DEG2RAD * aBearingInDegrees;

	// get current location in radians
	final double lat1 = DEG2RAD * getLatitude();
	final double lon1 = DEG2RAD * getLongitude();

	final double lat2 = Math.asin(Math.sin(lat1) * Math.cos(dist) + Math.cos(lat1)
				      * Math.sin(dist) * Math.cos(brng));
	final double lon2 = lon1
	    + Math.atan2(Math.sin(brng) * Math.sin(dist) * Math.cos(lat1), Math.cos(dist)
			 - Math.sin(lat1) * Math.sin(lat2));

	final double lat2deg = lat2 / DEG2RAD;
	final double lon2deg = lon2 / DEG2RAD;

	return new GeoPoint(lat2deg, lon2deg);
    }

    public static GeoPoint fromCenterBetween(final GeoPoint geoPointA, final GeoPoint geoPointB) {
	return new GeoPoint((geoPointA.getLatitude() + geoPointB.getLatitude()) / 2,
			    (geoPointA.getLongitude() + geoPointB.getLongitude()) / 2);
    }

    public String toDoubleString() {
	return new StringBuilder().append(this.mLatitude).append(",")
	    .append(this.mLongitude).append(",").append(this.mAltitude).toString();
    }

    public String toInvertedDoubleString() {
	return new StringBuilder().append(this.mLongitude).append(",")
	    .append(this.mLatitude).append(",").append(this.mAltitude).toString();
    }

    public int getLatitudeE6() {
	return (int) (this.getLatitude() * 1E6);
    }

    public int getLongitudeE6() {
	return (int) (this.getLongitude()* 1E6);
    }

}

/*
 * *********************************************************
 * Copyright (c) 2009 - 2011, DHBW Mannheim - Tigers Mannheim
 * Project: TIGERS - Sumatra
 * Date: 14.09.2011
 * Author(s): stei_ol
 * *********************************************************
 */


/**
 * Helper class for providing Angle math problems.
 * 
 * @author stei_ol
 */
public final class AngleMath
{
    /** */
    public static final double	PI  	= Math.PI;
    /** */
    public static final double	PI_TWO	= Math.PI * 2;
    /** */
    public static final double	PI_HALF	= Math.PI / 2.0;
    /** */
    public static final double	PI_QUART	= Math.PI / 4.0;
    /** */
    public static final double	PI_SQR	= Math.PI * Math.PI;
    /** */
    public static final double	PI_INV	= 1.0 / Math.PI;
    /** */
    public static final double	PI_TWO_INV= 1.0 / (Math.PI * 2);
				
    private static final double	DEG_RAD_FACTOR 	= 180;
    private static final double	HACKY_MAGIC_NUMBER = 0.000001;
    /** */
    public static final double	DEG_TO_RAD= PI / DEG_RAD_FACTOR;
    /** */
    public static final double	RAD_TO_DEG= DEG_RAD_FACTOR / PI;
				
    private static final double	ANGLE_EQUAL_TOLERANCE	= 0.01;
								
    private AngleMath()
    {
    }
	
	
    /**
     * Normalize angle, to make sure angle is in (-pi/pi] interval.<br>
     * New angle is returned, parameter stay unaffected.
     * 
     * @param angle
     * @author Malte
     * @return
     */
    public static double normalizeAngle(final double angle)
    {
	// Don't call this a hack! It's numeric!
	return (angle - (Math.round((angle / (PI_TWO)) - HACKY_MAGIC_NUMBER) * PI_TWO));
    }
	
	
    /**
     * Get the smallest difference between a1 and a2.
     * All values will be normalised.
     * 
     * @param a1 Angle 1
     * @param a2 Angle 2
     * @return Smallest difference. [-pi,pi]
     */
    public static double difference(final double a1, final double a2)
    {
	return normalizeAngle(normalizeAngle(a1) - normalizeAngle(a2));
    }
	
	
    /**
     * @param a1
     * @param a2
     * @param tolerance
     * @return
     */
    public static boolean isEqual(final double a1, final double a2, final double tolerance)
    {
	return Math.abs(difference(a1, a2)) < tolerance;
    }
	
	
    /**
     * @param a1
     * @param a2
     * @return
     */
    public static boolean isEqual(final double a1, final double a2)
    {
	return Math.abs(difference(a1, a2)) < ANGLE_EQUAL_TOLERANCE;
    }
	
	
    /**
     * Returns the shortest rotation for turning to target angle.
     * 
     * @param angle1
     * @param angle2
     * @return shortest rotation. [-pi;pi]
     */
    public static double getShortestRotation(final double angle1, final double angle2)
    {
	double rotateDist = 0;
		
	rotateDist = angle2 - angle1;
	if (rotateDist < -AngleMath.PI)
	    {
		rotateDist = AngleMath.PI_TWO + rotateDist;
	    }
	if (rotateDist > AngleMath.PI)
	    {
		rotateDist -= AngleMath.PI_TWO;
	    }
	return rotateDist;
    }
	
	
    /**
     * SINUS!
     * 
     * @author Malte
     * @param number
     * @return
     */
    public static double sin(final double number)
    {
	return Math.sin(number);
    }
	
	
    /**
     * COSINUS!
     * 
     * @author Malte
     * @param number
     * @return
     */
    public static double cos(final double number)
    {
	return Math.cos(number);
    }
	
	
    /**
     * TANGENS!
     * 
     * @author Malte
     * @param number
     * @return
     */
    public static double tan(final double number)
    {
	return Math.tan(number);
    }
	
	
    /**
     * @param number
     * @return (double) {@link Math#acos(double)}
     * @author Gero
     */
    public static double acos(final double number)
    {
	return Math.acos(number);
    }
	
	
    /**
     * @param number
     * @return (double) {@link Math#asin(double)}
     * @author Gero
     */
    public static double asin(final double number)
    {
	return Math.asin(number);
    }
	
	
    /**
     * @param deg The angle in degree that should be converted to radiant
     * @return The given angle in radiant
     * @author Gero
     */
    public static double deg2rad(final double deg)
    {
	return DEG_TO_RAD * deg;
    }
	
	
    /**
     * @param rad The angle in radiant that should be converted to degree
     * @return The given angle in degree
     * @author Gero
     */
    public static double rad2deg(final double rad)
    {
	return RAD_TO_DEG * rad;
    }
	
	
    /**
     * Tangens hyperbolicus
     * 
     * @param rad
     * @return Tangens hyperbolicus converted to double
     * @author AndreR
     */
    public static double tanh(final double rad)
    {
	return Math.tanh(rad);
    }
}

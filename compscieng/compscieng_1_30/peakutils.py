#
# https://github.com/atjacobs/PeakUtils adresinden alinmistir,
# bu kod Matlab dunyasinda iyi bilinen peakutils'in Python cevrimi
#
import numpy as np
from scipy import optimize
from scipy.integrate import simps

def indexes(y, thres=0.3, min_dist=1):
    '''Peak detection routine.

    Finds the peaks in *y* by taking its first order difference. By using
    *thres* and *min_dist* parameters, it is possible to reduce the number of
    detected peaks.

    Parameters
    ----------
    y : ndarray
        1D amplitude data to search for peaks.
    thres : float between [0., 1.]
        Normalized threshold. Only the peaks with amplitude higher than the
        threshold will be detected.
    min_dist : int
        Minimum distance between each detected peak. The peak with the highest
        amplitude is preferred to satisfy this constraint.

    Returns
    -------
    ndarray
        Array containing the indexes of the peaks that were detected
    '''
    thres *= np.max(y) - np.min(y)

    # find the peaks by using the first order difference
    dy = np.diff(y)
    peaks = np.where((np.hstack([dy, 0.]) < 0.)
                     & (np.hstack([0., dy]) > 0.)
                     & (y > thres))[0]

    if peaks.size > 1 and min_dist > 1:
        highest = peaks[np.argsort(y[peaks])][::-1]
        rem = np.ones(y.size, dtype=bool)
        rem[peaks] = False

        for peak in highest:
            if not rem[peak]:
                sl = slice(max(0, peak - min_dist), peak + min_dist + 1)
                rem[sl] = True
                rem[peak] = False

        peaks = np.arange(y.size)[~rem]

    return peaks


def centroid(x, y):
    '''Computes the centroid for the specified data.
    Refer to centroid2 for a more complete, albeit slower version.

    Parameters
    ----------
    x : ndarray
        Data on the x axis.
    y : ndarray
        Data on the y axis.

    Returns
    -------
    float
        Centroid of the data.
    '''
    return np.sum(x * y) / np.sum(y)

def centroid2(y, x=None, dx=1.):
    '''Computes the centroid for the specified data.
    Not intended to be used

    Parameters
    ----------
    y : array_like
        Array whose centroid is to be calculated.
    x : array_like, optional
        The points at which y is sampled.
    Returns
    -------
    (centroid, sd)
        Centroid and standard deviation of the data.
    '''
    yt = np.array(y)

    if x is None:
        x = np.arange(yt.size, dtype='float') * dx

    normaliser = simps(yt, x)
    centroid = simps(x * yt, x) / normaliser
    var = simps((x - centroid) ** 2 * yt, x) / normaliser
    return centroid, np.sqrt(var)

def gaussian(x, ampl, center, dev):
    '''Computes the Gaussian function.

    Parameters
    ----------
    x : float
        Point to evaluate the Gaussian for.
    a : float
        Amplitude.
    b : float
        Center.
    c : float
        Width.

    Returns
    -------
    float
        Value of the specified Gaussian at *x*
    '''
    return ampl * np.exp(-(x - center) ** 2 / (2 * dev ** 2))


def gaussian_fit(x, y):
    '''Performs a Gaussian fitting of the specified data.

    Parameters
    ----------
    x : ndarray
        Data on the x axis.
    y : ndarray
        Data on the y axis.

    Returns
    -------
    ndarray
        Parameters of the Gaussian that fits the specified data
    '''
    initial = [np.max(y), x[0], (x[1] - x[0]) * 5]
    params, pcov = optimize.curve_fit(gaussian, x, y, initial)
    return params[1]


def interpolate(x, y, ind=None, width=10, func=gaussian_fit):
    '''Tries to enhance the resolution of the peak detection by using
    Gaussian fitting, centroid computation or an arbitrary function on the
    neighborhood of each previously detected peak index.

    Parameters
    ----------
    x : ndarray
        Data on the x dimension.
    y : ndarray
        Data on the y dimension.
    ind : ndarray
        Indexes of the previously detected peaks. If None, indexes() will be
        called with the default parameters.
    width : int
        Number of points (before and after) each peak index to pass to *func*
        in order to encrease the resolution in *x*.
    func : function(x,y)
        Function that will be called to detect an unique peak in the x,y data.

    Returns
    -------
    ndarray :
        Array with the adjusted peak positions (in *x*)
    '''

    if ind is None:
        ind = indexes(y)

    out = []
    for slice_ in (slice(i - width, i + width) for i in ind):
        try:
            fit = func(x[slice_], y[slice_])
            out.append(fit)
        except Exception:
            pass

    return np.array(out)


if __name__ == "__main__": 
 
    centers = (30.5, 72.3)
    x = np.linspace(0, 120, 121)
    y = gaussian(x, 5, centers[0], 3) + gaussian(x, 7, centers[1], 10) + np.random.rand(x.size)
    indexes = indexes(y, thres=0.5, min_dist=30)
    print indexes

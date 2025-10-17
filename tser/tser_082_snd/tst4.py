import numpy as np
import pandas as pd
import scipy.io.wavfile
import statsmodels.api as sm

# --- DECOMPOSITION SETUP (MUST BE CORRECTED AND RE-RUN) ---
# Assuming 'ow.wav' is a single-channel file where ow is the data array.
# NOTE: It is best practice to get fs from the read operation.
fname = "/home/burak/Documents/classnotes/sk/2024/11/phonemes/ow.wav"
fs, ow = scipy.io.wavfile.read(fname)


# Initialize variables
N = len(ow) 
t = np.arange(N) / fs

# Define frequency range and basis functions
f_start = 100.0
f_end = 4000.0
frequencies_hz = np.linspace(f_start, f_end, 500)

# CRITICAL FIX: Generate all basis functions using NumPy broadcasting and create DataFrame at once.
# This prevents the 'PerformanceWarning' caused by repeated column insertion.
basis_functions = {}
for f in frequencies_hz:
    angular_freq = 2 * np.pi * f * t
    basis_functions[f'sin{f:.6f}'] = np.sin(angular_freq)
    basis_functions[f'cos{f:.6f}'] = np.cos(angular_freq)

# Create the DataFrame in a single step
X_unaugmented = pd.DataFrame(basis_functions)

# Separate dependent variable (signal amplitude)
Y = pd.Series(ow, name='wav').astype(np.float64) # Use the raw 'ow' array for Y

# CRITICAL FIX: Add the constant to the Pandas DataFrame directly.
# This ensures statsmodels retains the original column names (sin/cos terms) 
# and correctly names the intercept 'const'.
X_full = sm.add_constant(X_unaugmented, prepend=True) 

# Perform OLS Regression
model = sm.OLS(Y, X_full)
results = model.fit()


# --- RECOMBINATION (Refactored and Corrected) ---

# The full basis matrix used for reconstruction MUST match the matrix X_full used for fitting.
basis_matrix_full = X_full 
all_coeffs = results.params

# --- STEP 1: Determine significance ---
# is_significant: A boolean Series where True means the p-value is below the threshold.
is_significant = results.pvalues < 0.05

# significant_features: The list of column names (features) corresponding to the significant coefficients.
# This list now correctly includes 'const' and the original sin/cos names.
significant_features = results.pvalues[is_significant].index

# Diagnostic printout
print (f'tum {len(basis_matrix_full.columns)} filtre sonrasi {len(significant_features)}')


# --- STEP 2: Select only the significant basis functions and their coefficients ---
# Select the columns from the full basis matrix using the matching significant_features index.
selected_basis_matrix = basis_matrix_full.loc[:, significant_features]

# Select the corresponding coefficients.
selected_coeffs = all_coeffs[is_significant]


# --- STEP 3: Reconstruct the signal using dot product ---
# Reconstructed signal = (selected basis matrix) dot (selected coefficients)
# The result 'y' is a Pandas Series, approximating the original int16 data range.
reconstructed_signal_series = selected_basis_matrix.dot(selected_coeffs)

# Convert the reconstructed signal to a flat NumPy array.
y = np.array(reconstructed_signal_series).flatten() 

# --- STEP 4: Finalize and Save the WAV file ---
# The signal 'y' is already scaled to the original int16 range (approx. +/- 32767) 
# because the OLS was fit directly to the un-normalized int16 values.
# Convert directly to int16.
y_int16 = y.astype(np.int16)

# Write the final audio file
scipy.io.wavfile.write('/tmp/sound-out3.wav', fs, y_int16)

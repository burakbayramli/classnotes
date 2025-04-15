# Python code for paper "A Fast Triangle-Triangle Intersection Test" by Tomas Möller
# Converted into Python from its C version by Gemini 2.5
# https://fileadmin.cs.lth.se/cs/Personal/Tomas_Akenine-Moller/code/
import math

# Configuration Constants
USE_EPSILON_TEST = True
EPSILON = 0.000001

# --- Vector Operations ---

def cross_product(v1, v2):
  """Calculates the cross product of two 3D vectors."""
  return [
      v1[1] * v2[2] - v1[2] * v2[1],
      v1[2] * v2[0] - v1[0] * v2[2],
      v1[0] * v2[1] - v1[1] * v2[0]
  ]

def dot_product(v1, v2):
  """Calculates the dot product of two 3D vectors."""
  return v1[0] * v2[0] + v1[1] * v2[1] + v1[2] * v2[2]

def subtract_vectors(v1, v2):
  """Subtracts vector v2 from v1."""
  return [v1[0] - v2[0], v1[1] - v2[1], v1[2] - v2[2]]

def sort_pair(a, b):
  """Sorts two values so that a <= b."""
  if a > b:
    return b, a
  return a, b

# --- Coplanar Test Helpers ---

def edge_edge_test(V0, U0, U1, i0, i1, Ax, Ay):
  """
  Performs the edge-edge test based on Franklin Antonio's gem.
  Note: Requires Ax, Ay from the calling context (edge V0-V1).
  """
  Bx = U0[i0] - U1[i0]
  By = U0[i1] - U1[i1]
  Cx = V0[i0] - U0[i0]
  Cy = V0[i1] - U0[i1]
  f = Ay * Bx - Ax * By
  d = By * Cx - Bx * Cy
  if (f > 0 and 0 <= d <= f) or (f < 0 and f <= d <= 0):
    e = Ax * Cy - Ay * Cx
    if f > 0:
      if 0 <= e <= f:
        return True # Intersection detected
    else: # f < 0
      if f <= e <= 0:
        return True # Intersection detected
  return False # No intersection

def edge_against_tri_edges(V0, V1, U0, U1, U2, i0, i1):
  """Tests edge (V0,V1) against the edges of triangle (U0,U1,U2)."""
  Ax = V1[i0] - V0[i0]
  Ay = V1[i1] - V0[i1]
  # Test edge U0,U1 against V0,V1
  if edge_edge_test(V0, U0, U1, i0, i1, Ax, Ay):
    return True
  # Test edge U1,U2 against V0,V1
  if edge_edge_test(V0, U1, U2, i0, i1, Ax, Ay):
    return True
  # Test edge U2,U0 against V0,V1
  if edge_edge_test(V0, U2, U0, i0, i1, Ax, Ay):
    return True
  return False

def point_in_tri(V0, U0, U1, U2, i0, i1):
  """Checks if point V0 is inside the 2D projection of triangle (U0,U1,U2)."""
  a = U1[i1] - U0[i1]
  b = -(U1[i0] - U0[i0])
  c = -a * U0[i0] - b * U0[i1]
  d0 = a * V0[i0] + b * V0[i1] + c

  a = U2[i1] - U1[i1]
  b = -(U2[i0] - U1[i0])
  c = -a * U1[i0] - b * U1[i1]
  d1 = a * V0[i0] + b * V0[i1] + c

  a = U0[i1] - U2[i1]
  b = -(U0[i0] - U2[i0])
  c = -a * U2[i0] - b * U2[i1]
  d2 = a * V0[i0] + b * V0[i1] + c

  # Check signs including zero for points on edges
  # All non-negative OR all non-positive means inside or on boundary
  if d0 >= 0 and d1 >= 0 and d2 >= 0:
      return True
  if d0 <= 0 and d1 <= 0 and d2 <= 0:
      return True

  return False # Mixed signs mean outside

def coplanar_tri_tri(N, V0, V1, V2, U0, U1, U2):
  """Checks for intersection between two coplanar triangles."""
  A = [abs(N[0]), abs(N[1]), abs(N[2])]

  # First project onto an axis-aligned plane that maximizes the area
  # Compute indices: i0, i1.
  if A[0] > A[1]:
    if A[0] > A[2]:
      i0, i1 = 1, 2 # A[0] is greatest
    else:
      i0, i1 = 0, 1 # A[2] is greatest
  else: # A[0] <= A[1]
    if A[2] > A[1]:
      i0, i1 = 0, 1 # A[2] is greatest
    else:
      i0, i1 = 0, 2 # A[1] is greatest

  # Test all edges of triangle 1 against the edges of triangle 2
  if edge_against_tri_edges(V0, V1, U0, U1, U2, i0, i1): return 1
  if edge_against_tri_edges(V1, V2, U0, U1, U2, i0, i1): return 1
  if edge_against_tri_edges(V2, V0, U0, U1, U2, i0, i1): return 1

  # Finally, test if tri1 is totally contained in tri2 or vice versa
  if point_in_tri(V0, U0, U1, U2, i0, i1): return 1
  if point_in_tri(U0, V0, V1, V2, i0, i1): return 1

  return 0 # No intersection found


# --- Interval Computation Helper ---

def compute_intervals(VV0, VV1, VV2, D0, D1, D2, D0D1, D0D2, N1, V0, V1, V2, U0, U1, U2):
  """
  Computes the projection interval onto the intersection line.
  Returns a tuple: (status, val1, val2, val3, val4, val5)
  If status is 'coplanar', val1 holds the intersection result (0 or 1), others are None.
  If status is 'ok', returns ('ok', A, B, C, X0, X1).
  """
  if D0D1 > 0.0:
    A = VV2; B = (VV0 - VV2) * D2; C = (VV1 - VV2) * D2; X0 = D2 - D0; X1 = D2 - D1
  elif D0D2 > 0.0:
    A = VV1; B = (VV0 - VV1) * D1; C = (VV2 - VV1) * D1; X0 = D1 - D0; X1 = D1 - D2
  elif D1 * D2 > 0.0 or D0 != 0.0:
    A = VV0; B = (VV1 - VV0) * D0; C = (VV2 - VV0) * D0; X0 = D0 - D1; X1 = D0 - D2
  elif D1 != 0.0:
    A = VV1; B = (VV0 - VV1) * D1; C = (VV2 - VV1) * D1; X0 = D1 - D0; X1 = D1 - D2
  elif D2 != 0.0:
    A = VV2; B = (VV0 - VV2) * D2; C = (VV1 - VV2) * D2; X0 = D2 - D0; X1 = D2 - D1
  else:
    # Triangles are coplanar
    coplanar_result = coplanar_tri_tri(N1, V0, V1, V2, U0, U1, U2)
    # *** CORRECTED RETURN FOR COPLANAR CASE ***
    return 'coplanar', coplanar_result, None, None, None, None # Return 6 values

  # *** CORRECTED RETURN FOR NORMAL CASE ***
  return 'ok', A, B, C, X0, X1 # Return 6 values


# --- Main Intersection Function ---

def no_div_tri_tri_isect(V0, V1, V2, U0, U1, U2):
  """
  Triangle/triangle intersection test.
  Returns 1 if triangles intersect, 0 otherwise.
  """
  # Compute plane equation of triangle(V0,V1,V2)
  E1 = subtract_vectors(V1, V0)
  E2 = subtract_vectors(V2, V0)
  N1 = cross_product(E1, E2)
  d1 = -dot_product(N1, V0)

  # Put U0,U1,U2 into plane equation 1 to compute signed distances
  du0 = dot_product(N1, U0) + d1
  du1 = dot_product(N1, U1) + d1
  du2 = dot_product(N1, U2) + d1

  # Coplanarity robustness check
  if USE_EPSILON_TEST:
    # Check magnitude of normal vector - if close to zero, triangles are degenerate
    # This check was missing but can prevent issues with normalization/direction
    norm_N1_sq = dot_product(N1, N1)
    if norm_N1_sq < EPSILON * EPSILON :
       # print("Warning: Triangle 1 degenerate") # Optional warning
       # Degenerate triangles cannot intersect in a meaningful way according
       # to this algorithm's assumptions (non-zero plane normal needed).
       # Exact behavior might depend on requirements (e.g., edge/vertex touch?)
       # Returning 0 is safest based on algorithm's geometric assumptions.
       return 0

    if abs(du0) < EPSILON: du0 = 0.0
    if abs(du1) < EPSILON: du1 = 0.0
    if abs(du2) < EPSILON: du2 = 0.0

  du0du1 = du0 * du1
  du0du2 = du0 * du2

  if du0du1 > 0.0 and du0du2 > 0.0:
    return 0

  # Compute plane of triangle (U0,U1,U2)
  E1 = subtract_vectors(U1, U0)
  E2 = subtract_vectors(U2, U0)
  N2 = cross_product(E1, E2)
  d2 = -dot_product(N2, U0)

  # Put V0,V1,V2 into plane equation 2
  dv0 = dot_product(N2, V0) + d2
  dv1 = dot_product(N2, V1) + d2
  dv2 = dot_product(N2, V2) + d2

  if USE_EPSILON_TEST:
     # Check magnitude of normal vector 2
    norm_N2_sq = dot_product(N2, N2)
    if norm_N2_sq < EPSILON * EPSILON :
       # print("Warning: Triangle 2 degenerate") # Optional warning
       return 0 # Similar reasoning as for N1

    if abs(dv0) < EPSILON: dv0 = 0.0
    if abs(dv1) < EPSILON: dv1 = 0.0
    if abs(dv2) < EPSILON: dv2 = 0.0

  dv0dv1 = dv0 * dv1
  dv0dv2 = dv0 * dv2

  if dv0dv1 > 0.0 and dv0dv2 > 0.0:
    return 0

  # Compute direction of intersection line
  D = cross_product(N1, N2)

  # Compute and index to the largest component of D
  max_val = abs(D[0])
  index = 0
  bb = abs(D[1])
  cc = abs(D[2])
  if bb > max_val: max_val = bb; index = 1
  if cc > max_val: max_val = cc; index = 2

  # Check if D is near zero vector (planes parallel)
  # This check handles cases where du or dv are zero but N1/N2 cross product is zero
  if max_val < EPSILON:
      # Planes are parallel. Check if they are the same plane (coplanar)
      # This condition is implicitly handled if du0/du1/du2 or dv0/dv1/dv2 were all zero
      # If distances were non-zero but signs differed, they are parallel and non-intersecting
      # If distances were zero, compute_intervals will trigger coplanar check
      # Let compute_intervals handle the coplanar determination based on D values passed to it
      pass # Will proceed to compute_intervals which should detect coplanarity

  # Project onto L
  vp0 = V0[index]
  vp1 = V1[index]
  vp2 = V2[index]

  up0 = U0[index]
  up1 = U1[index]
  up2 = U2[index]

  # Compute interval for triangle 1
  # *** CORRECTED UNPACKING AND CHECKING ***
  results1 = compute_intervals(
      vp0, vp1, vp2, dv0, dv1, dv2, dv0dv1, dv0dv2,
      N1, V0, V1, V2, U0, U1, U2
  )
  status1 = results1[0]

  if status1 == 'coplanar':
      return results1[1] # Return the coplanar intersection result (0 or 1)

  # Unpack the rest if status is 'ok'
  _, a, b, c, x0, x1 = results1

  # Compute interval for triangle 2
  # *** CORRECTED UNPACKING AND CHECKING ***
  results2 = compute_intervals(
      up0, up1, up2, du0, du1, du2, du0du1, du0du2,
      N1, V0, V1, V2, U0, U1, U2
  )
  status2 = results2[0]

  if status2 == 'coplanar':
       # This implies an inconsistency or edge case if status1 was 'ok'
       # Return the coplanar result as the determinant state
       return results2[1]

  # Unpack the rest if status is 'ok'
  _, d_val, e, f, y0, y1 = results2


  # Check for interval overlap
  # Need robust handling for zero denominators in interval calculation
  # xxyy = (x0 * x1) * (y0 * y1) can be zero if any interval is zero length

  # Check for division by zero potential implicitly
  # If X0 == X1 or Y0 == Y1, the interval calculation might degenerate
  # The interval points represent t-values along the intersection line D
  # A = VV_at_crossing_plane + ...
  # isect1[0] = A*X0*X1*Y0*Y1 + B*X1*Y0*Y1
  # isect1[1] = A*X0*X1*Y0*Y1 + C*X0*Y0*Y1
  # Simplified forms:
  # isect1[0] = (A * X0 + B) * X1 * Y0 * Y1
  # isect1[1] = (A * X1 + C) * X0 * Y0 * Y1

  # Avoid actual division: work with scaled interval endpoints
  try:
    # Calculate terms, handle potential zero multipliers carefully
    xx = x0 * x1
    yy = y0 * y1
    xxyy = xx * yy

    tmp_a = a * xxyy
    isect1_0 = tmp_a + b * x1 * yy
    isect1_1 = tmp_a + c * x0 * yy

    tmp_d = d_val * xxyy
    isect2_0 = tmp_d + e * xx * y1
    isect2_1 = tmp_d + f * xx * y0

  except OverflowError:
      # Handle potential overflow with very large coordinate * distance products
      # This might indicate extreme geometry, returning no intersection might be safest
      return 0

  # Sort interval endpoints
  isect1_0, isect1_1 = sort_pair(isect1_0, isect1_1)
  isect2_0, isect2_1 = sort_pair(isect2_0, isect2_1)

  # Check for overlap
  # Check if interval 1 is completely to the left of interval 2 OR
  # Check if interval 2 is completely to the left of interval 1
  if isect1_1 < isect2_0 or isect2_1 < isect1_0:
    # Add epsilon check for near-touching intervals if strict non-overlap is needed
    # E.g. if abs(isect1_1 - isect2_0) < EPSILON or abs(isect2_1 - isect1_0) < EPSILON
    # For now, use strict inequality as per original C logic intent
    return 0 # No overlap

  return 1 # Overlap detected


# --- Main execution block (equivalent to C main) ---
if __name__ == "__main__":
  # Test case 1 (Should intersect - Result: 1)
  V0_1 = [0.0, 5.0, 0.0]
  V1_1 = [8.0, 0.0, 0.0]
  V2_1 = [0.0, 0.0, 0.0]

  U0_1 = [6.0, 8.0, 3.0]
  U1_1 = [6.0, 8.0, -2.0]
  U2_1 = [6.0, -4.0, -2.0]

  res1 = no_div_tri_tri_isect(V0_1, V1_1, V2_1, U0_1, U1_1, U2_1)
  print(res1) # Expected: 1

  # Test case 2 (Should not intersect - Result: 0)
  V0_2 = V0_1
  V1_2 = V1_1
  V2_2 = V2_1

  offset = 3.0
  U0_2 = [6.0 + offset, 8.0 + offset, 3.0 + offset]
  U1_2 = [6.0 + offset, 8.0 + offset, -2.0 + offset]
  U2_2 = [6.0 + offset, -4.0 + offset, -2.0 + offset]

  res2 = no_div_tri_tri_isect(V0_2, V1_2, V2_2, U0_2, U1_2, U2_2)
  print(res2) # Expected: 0

  # Example Coplanar Case (should intersect - small tri inside big one)
  V0_3 = [0.0, 0.0, 0.0]
  V1_3 = [1.0, 0.0, 0.0]
  V2_3 = [0.0, 1.0, 0.0]
  U0_3 = [0.2, 0.2, 0.0]
  U1_3 = [0.8, 0.2, 0.0]
  U2_3 = [0.2, 0.8, 0.0]
  res3 = no_div_tri_tri_isect(V0_3, V1_3, V2_3, U0_3, U1_3, U2_3)
  print(f"Coplanar intersecting: {res3}") # Expected 1

  # Example Coplanar Case (should not intersect - separate tris)
  V0_4 = V0_3
  V1_4 = V1_3
  V2_4 = V2_3
  U0_4 = [2.0, 2.0, 0.0]
  U1_4 = [3.0, 2.0, 0.0]
  U2_4 = [2.0, 3.0, 0.0]
  res4 = no_div_tri_tri_isect(V0_4, V1_4, V2_4, U0_4, U1_4, U2_4)
  print(f"Coplanar non-intersecting: {res4}") # Expected 0

  # Example Edge-Touching Coplanar Case
  V0_5 = [0.0, 0.0, 0.0]
  V1_5 = [1.0, 0.0, 0.0]
  V2_5 = [0.0, 1.0, 0.0]
  U0_5 = [1.0, 0.0, 0.0] # Share V1_5/U0_5
  U1_5 = [2.0, 0.0, 0.0]
  U2_5 = [1.0, 1.0, 0.0]
  res5 = no_div_tri_tri_isect(V0_5, V1_5, V2_5, U0_5, U1_5, U2_5)
  print(f"Coplanar edge touching: {res5}") # Expected 1 (as edges intersect)

  # Example Vertex-Touching Coplanar Case
  V0_6 = [0.0, 0.0, 0.0]
  V1_6 = [1.0, 0.0, 0.0]
  V2_6 = [0.0, 1.0, 0.0]
  U0_6 = [1.0, 1.0, 0.0]
  U1_6 = [2.0, 1.0, 0.0]
  U2_6 = [1.0, 2.0, 0.0]
  res6 = no_div_tri_tri_isect(V0_6, V1_6, V2_6, U0_6, U1_6, U2_6)
  print(f"Coplanar vertex touching: {res6}") # Expected 0 (by point_in_tri, edges don't cross)

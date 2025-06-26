def levenshtein(s1, s2):
    l1 = len(s1)
    l2 = len(s2)

    # Initialize a 2D matrix (list of lists) with zeros
    # This creates a l2+1 by l1+1 matrix
    matrix = [[0 for _ in range(l1 + 1)] for _ in range(l2 + 1)]

    # Fill the first row (distance from empty string to prefixes of s1)
    for i in range(l1 + 1):
        matrix[0][i] = i

    # Fill the first column (distance from empty string to prefixes of s2)
    for j in range(l2 + 1):
        matrix[j][0] = j

    # Populate the rest of the matrix
    for j in range(1, l2 + 1): # Iterate over characters of s2 (rows)
        for i in range(1, l1 + 1): # Iterate over characters of s1 (columns)
            cost = 0 if s1[i-1] == s2[j-1] else 1 # Cost of substitution

            matrix[j][i] = min(matrix[j][i-1] + 1,      # Deletion
                               matrix[j-1][i] + 1,      # Insertion
                               matrix[j-1][i-1] + cost) # Substitution/Match

    return matrix[l2][l1]

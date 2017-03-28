import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from PIL import Image

imname = 'sudoku81.JPG'
im = np.array(Image.open(imname).convert('L'))
plt.figure()
plt.imshow(im)
plt.gray()
x = plt.ginput(4)
print x

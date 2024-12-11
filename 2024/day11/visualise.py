
import matplotlib.pyplot as plt
import numpy as np

with open("./out.txt", "r") as fh:
    lines = fh.readlines()
    arr = np.array([int(l) for l in lines if int(l) < 3811]) # 3811 is the magical border
    
plt.plot(arr)
plt.show()


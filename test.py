import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
path="/home/changwan/SAR/Pulse_com.txt"

data=np.loadtxt(path)
data2=pd.DataFrame(data)

plt.plot(data2.loc[:,1])
plt.show()

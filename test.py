import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
path="/home/changwan/SAR/Pulse_com.txt"

data=np.loadtxt(path)
data2=pd.DataFrame(data)

plt.subplot(311)
plt.plot(data2.loc[:,0])

plt.subplot(312)
plt.plot(data2.loc[:,1])

#plt.subplot(313)
#!plt.plot(data2.loc[:,2])


plt.show()

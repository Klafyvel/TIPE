import matplotlib.pyplot as pl
import numpy as np

X = range(0, 10000)
Y = []
e = ""
while True:
    try:
        e = input()
        Y.append(int(e))
    except EOFError:
        break

# Y.sort()

M = max(Y)
T = range(M+1)
D = np.zeros(M+1)
for i in range(10000):
    D[Y[i]] += 1

V = np.zeros(M+1)
for i, d in enumerate(D):
    if i < M - 1:
        V[i+1] = V[i] + D[i]
pl.plot(T, V)
pl.show()

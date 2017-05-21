import matplotlib.pyplot as pl
import numpy as np
import random as rd


pl.close('all')

K = 2
N = 10
Beta = 0.6

def plotEdges(V, M, s=[]):
    for i in range(N+1):
        for j in range(N+1):
            if M[i,j] and ((i,j) not in s):
                pl.plot([V[0][i], V[0][j]], [V[1][i], V[1][j]], 'b')
            elif M[i,j]:
                pl.plot([V[0][i], V[0][j]], [V[1][i], V[1][j]], c='red', linewidth=4)

def plotVertices(V, s=N/4, l='blue'):
    pl.plot(V[0][:int(s)],V[1][:int(s)],'o', c='orange', markersize=20)
    pl.plot(V[0][int(s):N],V[1][int(s):N],'o', c='green', markersize=20)
    pl.plot(V[0][N:],V[1][N:],'o', c=l, markersize=20)
        
V = [[np.cos(t*2*np.pi/N) for t in range(N)]+[0], 
    [np.sin(t*2*np.pi/N) for t in range(N)]+[0]]

M = np.zeros((N+1,N+1), bool)

for i in range(N):
    M[i,N] = M[N, i] = True


plotEdges(V, M)
plotVertices(V, s = 3*N/4, l='orange')
pl.axis('equal')
pl.show()
import matplotlib.pyplot as pl
import numpy as np
import random as rd


pl.close('all')

K = 2
N = 10
Beta = 0.6

def plotEdges(M,s=[]):
    for i in range(N):
        for j in range(N):
            if M[i,j] and ((i,j) not in s):
                pl.plot([V[0][i], V[0][j]], [V[1][i], V[1][j]], 'b')
            elif M[i,j]:
                pl.plot([V[0][i], V[0][j]], [V[1][i], V[1][j]], c='red', linewidth=4)

def plotVertices(V, s=None):
    if s == None:
        pl.plot(V[0],V[1],'o', c='orange', markersize=20)
    else:
        pl.plot(V[0][:s]+V[0][s+1:],V[1][:s]+V[1][s+1:],'o', c='orange', markersize=20)
        pl.plot([V[0][s]],[V[1][s]],'o', c='red', markersize=20)
        
V = [[np.cos(t*2*np.pi/N) for t in range(N)], 
    [np.sin(t*2*np.pi/N) for t in range(N)]]

M = np.zeros((N,N), bool)

for i in range(N):
    for j in range(1,K+1):
        M[i,(i+j)%N] = True
        M[(i-j)%N,i] = True

for i in range(N):
    for j in range(1,K+1):
        n = rd.random()
        if n <= Beta:
            M[i,(i+j)%N] = M[(i-j)%N,i] = False
            k = rd.randint(0,N-1)
            while M[i,k]:
                k = rd.randint(0,N-1)
            M[i,k] = M[k,i] = True

plotEdges(M)
plotVertices(V)
pl.axis('equal')
pl.show()
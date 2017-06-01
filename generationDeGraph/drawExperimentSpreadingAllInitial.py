import matplotlib.pyplot as pl
from matplotlib2tikz import save as tikz_save
import numpy as np
import sqlite3
import json

# r_spreading_between_500_50_50_50_25_500_2_1
graph_size = 500
nb_gen = 50
k = 50
max_spread_step = 500
conn = sqlite3.connect('experiments.sqlite')
beta = 50

# size = 100
# l = [(1,1), (1,3), (3,1)]

size = 50
l = [(2,1)]

for a,b in l:
    X = np.arange(1,size)
    pl.close('all')
    fig = pl.figure()
    ax = fig.add_subplot(111)
    ax.set_title('Taille du graphe={} Nb gen={} K={}, q={}, Beta ={}\\%'
        .format(graph_size,nb_gen,k,round(b/(a+b), 3),beta))
    ax.set_ylabel("Proportion finale")
    ax.set_xlabel("Proportion initiale (\\%)")
    
    
    Y = [np.zeros(size) for _ in range(3)]
    Y_high = [np.zeros(size) for _ in range(3)]
    Y_low = [np.zeros(size) for _ in range(3)]
    
    corres = ["between", "degree", "random"]
    x_50 = [0,0,0]
    pl.grid()
    pl.plot(X,X/100, '--', color="gray", label="Identité", lw=2.5)
    for i in range(3):
        for x in X:
            cursor = conn.execute(
                "SELECT value FROM r_spreading_{}_".format(corres[i])+
                "{graph_size}_{beta}_{k}_{nb_gen}_{x}_{max_spread_step}_{a}_{b}"
                .format(**locals()))
            rows = cursor.fetchall()
            v = 0
            n = len(rows)
            for row in rows:
                v += json.loads(row[0])['prop_spread'][-1]
            Y[i][x] = v/n
            if abs(0.5-Y[i][x]) < abs(Y[i][x_50[i]]-0.5):
                x_50[i] = x
            v = 0
            for row in rows:
                v += (Y[i][x] - json.loads(row[0])['prop_spread'][-1])**2 
            v = (v/n)**(1/2)
            Y_high[i][x] = Y[i][x] + v
            Y_low[i][x] = Y[i][x] - v

        pl.plot(X,Y[i][1:], label="Propagation finale {}".format(corres[i]), lw=2.5)
    pl.axhline(0.5, xmin=0, xmax=100, c='orange', lw=2.5)
    pl.legend(loc="lower right")
    tikz_save("resultats/all_finale_f_initiale_q{}_Beta{}_ec.tex"
        .format(int(b/(a+b)*100), beta))
    pl.plot()
    pl.show()
conn.close()
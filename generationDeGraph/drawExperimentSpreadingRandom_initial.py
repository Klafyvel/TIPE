import matplotlib.pyplot as pl
import numpy as np
import sqlite3
import json


graph_size = 500
nb_gen = 100
k = 50
max_spread_step = 500
for a,b in [(1,1), (1,3), (3,1)]:
    for beta in [0,25,50,75,100]:
        X = np.arange(1,100)
        conn = sqlite3.connect('experiments.sqlite')
        pl.close('all')
        fig = pl.figure()
        ax = fig.add_subplot(111)
        ax.set_title('Taille du graphe={} Nb gen={} K={}, q={}, Beta ={}%'
            .format(graph_size,nb_gen,k,b/(a+b),beta))
        ax.set_ylabel("Proportion finale")
        ax.set_xlabel("Proportion initiale (%)")
        
        
        Y = np.zeros(100)
        Y_high = np.zeros(100)
        Y_low = np.zeros(100)
        
        
        x_50 = 0
        
        for x in X:
            cursor = conn.execute(
                "SELECT value FROM r_spreading_random_"+
                "{graph_size}_{beta}_{k}_{nb_gen}_{x}_{max_spread_step}_{a}_{b}"
                .format(**locals()))
            rows = cursor.fetchall()
            v = 0
            n = len(rows)
            for row in rows:
                v += json.loads(row[0])['prop_spread'][-1]
            Y[x] = v/n
            if abs(0.5-Y[x]) < abs(Y[x_50]-0.5):
                x_50 = x
            v = 0
            for row in rows:
                v += (Y[x] - json.loads(row[0])['prop_spread'][-1])**2 
            v = (v/n)**(1/2)
            Y_high[x] = Y[x] + v
            Y_low[x] = Y[x] - v
        
        pl.plot(X,Y[1:], 'b', label="Propagation finale")
        pl.plot(X, Y_high[1:], 'r--', label="Écart-type")
        pl.plot(X, Y_low[1:], 'r--')
        pl.plot(X,X/100, 'g--', label="Identité")
        
        pl.axvline(x_50, c='orange')
        pl.axhline(0.5, c='orange')
        pl.text(x_50+1, 0.1, str(x_50)+" %",
            bbox=dict(facecolor='orange', alpha=0.95), 
            size="large",
            color="white",
            fontweight="bold")
        
        conn.close()
        
        pl.legend(loc="best")
        pl.grid()
        pl.savefig("resultats/random_finale_f_initiale_q{}_Beta{}_ec"
            .format(int(b/(a+b)*100), beta))

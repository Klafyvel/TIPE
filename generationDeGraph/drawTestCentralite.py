import csv
import matplotlib.pyplot as pl
import numpy as np
import sqlite3
import json

"""
Script pour mesurer la répartition de le centralité des noeuds en fonction du
rang du noeud.

"""

graph_size = 500
nb_gen = 100
k = 50

X = np.arange(graph_size)
Y = np.zeros(graph_size)

Y_deviation = np.zeros(graph_size)
Y_high = np.zeros(graph_size)
Y_low = np.zeros(graph_size)


def process_json_mean(j):
    t = json.loads(j)['btc']
    for x,i in enumerate(t):
        Y[x] += i
        Y_high[x] = max(Y_high[x],i)
        if Y_low[x] == 0:
            Y_low[x] = i
        else:
            Y_low[x] = min(Y_low[x],i)

def process_standard_deviation(j):
    t = json.loads(j)['btc']
    for x,i in enumerate(t):
        Y_deviation[x] += (i - Y[x])**2

conn = sqlite3.connect('experiments.sqlite')
cursor = conn.execute("SELECT * FROM betweenness_wattsstrogatz_{}_{}_{}".format(graph_size, k, nb_gen))
rows = cursor.fetchall()
for row in rows:
    process_json_mean(row[1])
Y = [y/nb_gen for y in Y]

for row in rows:
    process_standard_deviation(row[1])
Y_deviation = [(y/nb_gen)**(1/2) for y in Y_deviation]
conn.close()
Y_shigh = [Y[x]+Y_deviation[x] for x in X]
Y_slow = [Y[x]-Y_deviation[x] for x in X]

pl.close('all')
fig = pl.figure()
ax = fig.add_subplot(111)
fig.suptitle("Centralité moyenne d'un noeud", fontweight='bold')
ax.set_title('Taille du graphe={} Nb gen={} K={}'.format(graph_size,nb_gen,k))
ax.set_ylabel("Centralité")
ax.set_xlabel("Rang du noeud")
pl.plot(X, Y, 'o', label="Centralité moyenne")
#pl.plot(X, Y_high)
#pl.plot(X, Y_low)
pl.plot(X, Y_shigh, 'r', label="Écart-type")
pl.plot(X, Y_slow, 'r')
#pl.plot(X, Y_deviation)
pl.legend(loc="best")
pl.show()

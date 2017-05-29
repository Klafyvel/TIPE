import matplotlib.pyplot as pl
import numpy as np
import sqlite3
import json


graph_size = 500
nb_gen = 100
k = 50
max_spread_step = 500

X = np.arange(max_spread_step)
conn = sqlite3.connect('experiments.sqlite')
pl.close('all')
fig = pl.figure()
ax = fig.add_subplot(111)
fig.suptitle(
    "Avancement de la propagation pour une distribution initiale aléatoire.", 
    fontweight='bold')
ax.set_title('Taille du graphe={} Nb gen={} K={}'.format(graph_size,nb_gen,k))
ax.set_ylabel("Proportion")
ax.set_xlabel("Étape")


def process_json_mean(j, Y, Y_low, Y_high):
    t = json.loads(j)['prop_spread']
    for x,i in enumerate(t):
        Y[x] += i
        Y_high[x] = max(Y_high[x],i)
        if Y_low[x] == 0:
            Y_low[x] = i
        else:
            Y_low[x] = min(Y_low[x],i)

def process_standard_deviation(j, Y_deviation, Y):
    t = json.loads(j)['prop_spread']
    for x,i in enumerate(t):
        Y_deviation[x] += (i - Y[x])**2

def process_one(init):
    
    Y = np.zeros(max_spread_step)
    
    Y_deviation = np.zeros(max_spread_step)
    Y_high = np.zeros(max_spread_step)
    Y_low = np.zeros(max_spread_step)

    cursor = conn.execute(
        "SELECT value FROM r_spreading_random_"+
        "{graph_size}_{beta}_{k}_{nb_gen}_{x}_{max_spread_step}_{a}_{b}".format(
            graph_size, nb_gen, int(init*10), max_spread_step))
    rows = cursor.fetchall()
    for row in rows:
        process_json_mean(row[1], Y, Y_low, Y_high)
    Y = [y/nb_gen for y in Y]
    pl.plot(X, Y, label="Proportion initiale {}%".format(init*10))

for i in range(6,15):
    process_one(i/2)
conn.close()

pl.legend(loc="best")
pl.grid()
pl.show()

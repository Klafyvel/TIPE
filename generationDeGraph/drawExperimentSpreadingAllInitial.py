import matplotlib.pyplot as pl
from matplotlib2tikz import save as tikz_save
import numpy as np
import sqlite3
import json

graph_size = 500
nb_gen = 100
k = 50
max_spread_step = 500
conn = sqlite3.connect('experiments.sqlite')

size = 100
l = [(1,1), (1,3), (3,1)]

# size = 50
# l = [(2,1)]
X = np.arange(1,size)
n = 4
Y = [np.zeros(size) for _ in range(n)]
Y_high = [np.zeros(size) for _ in range(n)]
Y_low = [np.zeros(size) for _ in range(n)]
corres = [
    (50, "degree", "Degré", "red"),
    (50, "random", "Aléatoire", "green"),
    (25, "between", "Centralité $\\beta=\\frac{1}{4}$", "m"),
    (75, "between", "Centralité $\\beta=\\frac{3}{4}$", "b"),
]

def affiche(a, b):
    pl.close('all')
    fig = pl.figure()
    ax = fig.add_subplot(111)
    ax.set_title('Taille du graphe:{} Échantillon:{} K={}, q=$\\frac'
        .format(graph_size,nb_gen,k)+'{'+str(b)+'}{'+str(a+b)+'}$')
    ax.set_ylabel("Proportion finale")
    ax.set_xlabel("Proportion initiale")

    pl.grid()
    pl.plot(X/100,X/100, '--', color="gray", label="Identité", lw=2.5)
    for i,(beta, name, label, color) in enumerate(corres):
        for x in X:
            cursor = conn.execute(
                "SELECT value FROM r_spreading_{}_".format(name)+
                "{}_{}_{}_{}_{}_{}_{}_{}"
                .format(graph_size, beta, k, nb_gen, x, max_spread_step, a, b))
            rows = cursor.fetchall()
            v = 0
            n = len(rows)
            for row in rows:
                v += json.loads(row[0])['prop_spread'][-1]
            Y[i][x] = v/n
            v = 0
            for row in rows:
                v += (Y[i][x] - json.loads(row[0])['prop_spread'][-1])**2 
            v = (v/n)**(1/2)
            Y_high[i][x] = Y[i][x] + v
            Y_low[i][x] = Y[i][x] - v

        pl.plot(X/100,Y[i][1:], label="{}".format(label), lw=2.5, color=color)

    # for i in range(3):
        # pl.plot(X, Y_high[i][1:], 'r--', lw=2)
        # pl.plot(X, Y_low[i][1:], 'r--', lw=2)
    # pl.axhline(0.5, xmin=0, xmax=100, c='orange', lw=2.5)
    pl.legend(loc="lower right")
    tikz_save("resultats/all_finale_f_initiale_q{}.tex"
        .format(int(b/(a+b)*100), beta),
        figurewidth="\\textwidth",
        figureheight=".33\\textheight")
    # pl.show()
for a,b in l:
    affiche(a,b)
conn.close()
import csv
import matplotlib.pyplot as pl
import numpy as np
import sqlite3
import json


graph_size = 500
nb_gen = 100
k = 50
max_spread_step = 500
a,b = 3,1
beta = 60

X = np.arange(1,100)
conn = sqlite3.connect('experiments.sqlite')
pl.close('all')
fig = pl.figure()
ax = fig.add_subplot(111)
fig.suptitle("Propagation finale pour une distribution initiale aléatoire en fonction de la proportion initiale.", fontweight='bold')
ax.set_title('Taille du graphe={} Nb gen={} K={}, q={}, Beta ={}%'.format(graph_size,nb_gen,k,b/(a+b),beta))
ax.set_ylabel("Proportion finale")
ax.set_xlabel("Proportion initiale (%)")


Y = np.zeros(100)
Y_high = np.zeros(100)
Y_low = np.zeros(100)


x_50 = 0

for x in X:
    cursor = conn.execute(
        "SELECT value FROM spreading_random_{graph_size}_{beta}_{k}_{nb_gen}_{x}_{max_spread_step}_{a}_{b}".format(
                **locals()))
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
# pl.plot(X, Y_high[1:], 'r--', label="Écart-type")
# pl.plot(X, Y_low[1:], 'r--')
pl.plot(X,X/100, 'g--', label="Identité")

pl.axvline(x_50, c='orange')
pl.axhline(0.5, c='orange')
pl.text(x_50+1, 0.1, str(x_50)+" %",
    bbox=dict(facecolor='orange', alpha=0.95), 
    size="large",
    color="white",
    fontweight="bold")


# def process_json_mean(j, Y, Y_low, Y_high):
#     t = json.loads(j)['prop_spread']
#     for x,i in enumerate(t):
#         Y[x] += i
#         Y_high[x] = max(Y_high[x],i)
#         if Y_low[x] == 0:
#             Y_low[x] = i
#         else:
#             Y_low[x] = min(Y_low[x],i)

# def process_standard_deviation(j, Y_deviation, Y):
#     t = json.loads(j)['prop_spread']
#     for x,i in enumerate(t):
#         Y_deviation[x] += (i - Y[x])**2

# def process_one(init):
    
#     Y = np.zeros(max_spread_step)
    
#     Y_deviation = np.zeros(max_spread_step)
#     Y_high = np.zeros(max_spread_step)
#     Y_low = np.zeros(max_spread_step)

#     cursor = conn.execute("SELECT * FROM spreading_random_{}_{}_{}_{}".format(graph_size, nb_gen, int(init*10), max_spread_step))
#     rows = cursor.fetchall()
#     for row in rows:
#         process_json_mean(row[1], Y, Y_low, Y_high)
#     Y = [y/nb_gen for y in Y]

#     # for row in rows:
#     #     process_standard_deviation(row[1], Y_deviation, Y)
#     # Y_deviation = [(y/nb_gen)**(1/2) for y in Y_deviation]
#     # Y_shigh = [Y[x]+Y_deviation[x]/2 for x in X]
#     # Y_slow = [Y[x]-Y_deviation[x]/2 for x in X]


#     pl.plot(X, Y, 'o', label="Proportion initiale {}%".format(init*10))
#     # pl.plot(X, Y_high)
#     # pl.plot(X, Y_low)
#     # pl.plot(X, Y_shigh, 'r--', label="Écart-type")
#     # pl.plot(X, Y_slow, 'r--')
#     #pl.plot(X, Y_deviation)


# for i in range(6,15):
#     process_one(i/2)
conn.close()

pl.legend(loc="best")
pl.grid()
pl.show()

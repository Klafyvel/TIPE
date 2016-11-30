import csv
import matplotlib.pyplot as pl
import numpy as np


"""
Script pour mesurer la répartition de le centralité des noeuds en fonction du
rang du noeud.

"""

X = np.arange(500)
Y = []
with open("testCentralite.csv", "r") as f:
    csv_reader = csv.reader(f)
    for row in csv_reader:
        y = row[0]
        y = float(y)
        Y.append(y)
# pl.xkcd()
pl.title("Centralité moyenne d'un noeud en fonction de son rang.\nÉchantillon : 100 graphes.")
pl.ylabel("Centralité")
pl.xlabel("Rang du noeud")
pl.plot(X, Y, 'o')
pl.show()

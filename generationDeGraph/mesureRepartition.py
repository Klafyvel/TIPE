import csv
import matplotlib.pyplot as pl
import numpy as np


"""
Script pour mesurer la répartition des degrés des noeuds en fonction du rang
du noeud.

"""

X = np.arange(1000)
Y = np.zeros(1000)
with open("measure.csv", "r") as f:
    csv_reader = csv.reader(f)
    for row in csv_reader:
        i, y, = row
        i, y = int(i), float(y)
        Y[i] = y
pl.plot(X, Y)
pl.show()

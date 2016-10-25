import json
import graphviz as gv

g = gv.Graph(format="svg")
j = []
with open("graph.json", "r") as f:
    j = json.load(f)
size = len(j)
done = {}
for i in range(size):
    g.node(str(i))
for i, n in enumerate(j):
    for k in n["link"]:
        if not done.get((min(i, k), max(i, k)), False) :
            g.edge(str(i), str(k))
            done[(min(i, k), max(i, k))] = True

g.render("img/graph")

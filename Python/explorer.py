def is_connex(graph):
    goal = len(graph)
    if goal is 0:
        return True
    active = {list(graph.keys())[0]}
    found = {}
    while len(found) < goal and len(active) > 0:
        found += len(active)
        new = set()
        for v in active:
            for c in v.l:
                if c not in found:
                    new.add(c)
        active = new

    return len(found) is goal

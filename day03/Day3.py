def traverse(stepi, stepj):
    i, j = 0, 0
    treeCount = 0
    while  j < len(lines)-1:
        i = (i + stepi) % len(lines[0])
        j += stepj
        if lines[j][i] == '#':
            treeCount += 1
    return treeCount

with open("input.txt") as f:
    data = f.read()
lines = data.splitlines()

print(traverse(3, 1))
print(traverse(1, 1) * traverse(3,1) * traverse(5,1) * traverse(7,1)* traverse(1, 2))

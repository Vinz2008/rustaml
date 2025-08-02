import sys
import matplotlib.pyplot as plt

arg_filename = sys.argv[1]

arg_file = open(arg_filename)

nbs = []

for line in arg_file.readlines():
    nb = int(line)
#    print(nb)
    nbs.append(nb)

arg_file.close()

nb_hashmap = {}

for nb in nbs:
    if nb_hashmap.get(nb) == None:
        nb_hashmap[nb] = 1
    else:
        nb_hashmap[nb] += 1

nbs = []
nbs_count = []    

for k, v in nb_hashmap.items():
    nbs.append(k)
    nbs_count.append(v)

plt.plot(nbs, nbs_count, 'ro')
plt.show()


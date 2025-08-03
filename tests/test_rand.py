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

# plt.plot(nbs, nbs_count, 'ro')
# plt.show()




digits = [0 for _ in range(9)]

for nb in nbs:
    #digit = int(str(abs(nb))[:1])
    digit = int(str(abs(nb))[0])
    #print(digit)
    digits[digit-1] += 1


# plt.plot(range(1, 10), digits)
# plt.show()


int64_min = -9223372036854775807
int64_max = 9223372036854775807

range_int64 = (int64_max - int64_min)

parts_nb = 10
step = range_int64/parts_nb

ranges = []
for i in range(1, parts_nb+1):
    start = int64_min + step * (i -1)
    end = int64_min + step * i
    print((start, end))
    ranges.append((start, end))


ranges_count = [0 for _ in range(parts_nb)]

for nb in nbs:
    for (idx, (r_start, r_end)) in enumerate(ranges):
        if r_start <= nb and nb <= r_end:
            ranges_count[idx] += 1

print(ranges_count)

ranges_str = []
for r in ranges:
    ranges_str.append(str(r))
print(ranges_str)

plt.plot(ranges_str, ranges_count)
plt.ylim(bottom=0)
plt.show()
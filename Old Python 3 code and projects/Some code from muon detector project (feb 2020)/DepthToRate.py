import matplotlib.pyplot as plt
from matplotlib.ticker import EngFormatter
from matplotlib import rc
from WeightedMean import weightedmean

rc('text', usetex=True)
rc('font', **{'family': 'sans-serif', 'sans-serif': ['Helvetica']})

# TODO
# Triggers per second or rate to depth?
# How to calculate error in triggers per second?
# Remove italics in y axis, without using rc as it introduces delay
# Change size of dots in matplotlib
# read file straight into latex table

with open('TBSTRFile', 'r') as DTRFile:
    DTRData = DTRFile.readlines()
    depth, rate, error_in_rate, n, triggers, exposure_time = zip(*[line.split() for line in DTRData])
    DTRFile.close()

depth, rate, error_in_rate, n = list(map(int, depth)), list(map(int, rate)), list(map(int, error_in_rate)), list(map(int, n))
list(map(int, triggers)), list(map(float, exposure_time))
triggers_per_second = []

for i in range(0, len(triggers)):
    triggers_per_second.append(float(triggers[i]) / float(exposure_time[i]))

weightedmean(depth, rate, error_in_rate, n)

ylim = []
for i in range(0, len(rate)):
    ylim.append(rate[i] + error_in_rate[i])

axis_fontsize, title_fontsize = 10, 12

fig, ax = plt.subplots(figsize=(8, 6))
ax.plot(depth, rate)
ax.grid(True)
formatter0 = EngFormatter()
ax.yaxis.set_major_formatter(formatter0)
ax.errorbar(depth, rate, yerr=error_in_rate, xerr=0.0, fmt='r-o', ecolor='steelblue', capsize=2)
ax.set_xlim(min(depth) - 5, max(depth) + 5)
# ax.set_ylim(min(rate) - 2 * error_in_rate[rate.index(min(rate))], max(rate) + 2 * error_in_rate[rate.index(max(rate))])
ax.set_ylim(0.5 * min(ylim), 1.1 * max(ylim))
ax.set_title('Depth of Tunnel to Rate of Cosmic Rays Detected, incomplete', fontsize=title_fontsize)
ax.set_xlabel('Depth (m)', fontsize=axis_fontsize)
ax.set_ylabel(r'Rate\/(day$^{-1}$)', fontsize=axis_fontsize)
plt.savefig('/Users/domgr/OneDrive - University of Surrey/Placement/Thesis/Images/DepthToRate.png')
plt.show()
# ax.set_title('Depth of Tunnel to Rate of Cosmic Rays Detected, incomplete', fontname='Times New Roman', fontsize=13)
# ax.set_xlabel('Depth (m)', fontname='Times New Roman', fontsize=9)
# ax.set_ylabel(r'$Rate\/(day^{-1}$)', fontname='Times New Roman', fontsize=9)
# Take repeated depths and calculated the weighted mean of the values via the error
# output_path = os.path.abspath('\Users\domgr\OneDrive - University of Surrey\Placement\Thesis\Images')
# Unitemps6789
'''
for dep in depth:

    indices = []
    p, top, bottom, n_new = -1, 0, 0, 0

    if depth.count(dep) >= 2:

        for element in depth:
            p += 1
            if element == dep:
                indices.append(p)

        for i in indices:
            top += rate[i] / (error_in_rate[i] ** 2)
            bottom += 1 / (error_in_rate[i] ** 2)
            n_new += n[i]

        for i in indices:

            if indices.index(i) == 0:

                rate[i] = top / bottom
                error_in_rate[i] = math.sqrt(1 / bottom)
                n[i] = n_new

            elif indices.index(i) != 0:

                del rate[i]
                del error_in_rate[i]
                del depth[i]
                del n[i]

                for t in indices:
                    if t > i:
                        indices[indices.index(t)] -= 1
'''
'''
for dep in depth:
    i = -1
    for deptwo in depth:
        i += 1
        if dep == deptwo and depth.index(dep) != i:
            depindex = depth.index(dep)
            deptwoindex = depth.index(deptwo)
            top = ((rate[depindex])/(error_in_rate[depindex])**2) + ((rate[i])/(error_in_rate[i])**2)
            bottom = 1/((error_in_rate[depindex])**2)+ 1/((error_in_rate[i])**2)
            newrate = top/bottom
            newerrorinrate = 1/bottom
            n[depindex] = n[depindex] + n[i]
            del depth[i]
            del rate[i]
            del error_in_rate[i]
            del n[i]
            rate[depindex] = newrate
            error_in_rate[depindex] = newerrorinrate
'''
'''
rep = []
for dep in depth:
    i = -1
    t = False
    for deptwo in depth:
        i += 1
        if dep == deptwo and depth.index(dep) != i and [dep, rate[i], error_in_rate[i], n[i]] not in rep:
            rep.append([dep, rate[i], error_in_rate[i], n[i]])
            t = True
        if t is True and [dep, rate[depth.index(dep)], error_in_rate[depth.index(dep)], n[depth.index(dep)]] not in rep:
            rep.append([dep, rate[depth.index(dep)], error_in_rate[depth.index(dep)], n[depth.index(dep)]])
rep.sort()
'''
'''
#smallest = 0

for dep in depth:

    indices = []
    p, top, bottom, n_new = -1, 0, 0, 0

    if depth.count(dep) >=2:

        for element in depth:
            p += 1
            if element == dep:
                indices.append(p)

        for i in indices:

            top += depth[i]/((error_in_rate[i])**2)
            bottom += 1/(error_in_rate[i])**2
            n_new += n[i]

        for i in indices:

            if indices.index(i) == 0:
                rate[i] = top/bottom
                error_in_rate[i] = 1/bottom
                n[i] = n_new

            #elif min(indices) < smallest and indices.index(i) != 0:
                #smallest = min(indices)

            elif indices.index(i) != 0:

                del rate[i]
                del error_in_rate[i]
                del depth[i]
                del n[i]

                for t in indices:
                    if t > i:
                        indices[indices.index(t)] -= 1


#depth = depth[0:smallest-1]
#rate = rate[0:smallest-1]
#error_in_rate = error_in_rate[0:smallest-1]
#n = n[0:smallest-1]
'''
'''

cosmicraysim -g whiteball_tunnel_longitudinal.geo -n 5000

n = 5000

{
 name: "VARIABLE",
 index: "simconstants",
 world_box_width: "140.0*m", 
 world_box_length: "140.0*m",
 world_box_height: "100.0*m",
 tunnel_depth: "10.0*m"         !This was being varied!
 transverse_config: "90.0"

 shaft_diameter: "3.0*m"
 shaft_distance: "10.0*m"
}
'''

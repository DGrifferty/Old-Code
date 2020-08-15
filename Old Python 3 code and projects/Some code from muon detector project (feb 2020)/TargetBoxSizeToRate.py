import matplotlib.pyplot as plt
from matplotlib.ticker import EngFormatter
from matplotlib import rc
from WeightedMean import weightedmean

rc('text', usetex=True)
rc('font', **{'family': 'sans-serif', 'sans-serif': ['Helvetica']})

with open('TBSTRFile', 'r') as TBSTRFile:
    TBSTRData = TBSTRFile.readlines()
    Target_Box_Size, rate, error_in_rate, n, triggers, exposure_time = zip(*[line.split() for line in TBSTRData])
    TBSTRFile.close()

Target_Box_Size, rate, error_in_rate, n = list(map(int, Target_Box_Size)), list(map(int, rate)), list(map(int, error_in_rate)), list(map(int, n))
list(map(int, triggers)), list(map(float, exposure_time))
triggers_per_second = []

for i in range(0, len(triggers)):
    triggers_per_second.append(float(triggers[i]) / float(exposure_time[i]))

weightedmean(Target_Box_Size, rate, error_in_rate, n)

ylim = []
for i in range(0, len(rate)):
    ylim.append(rate[i] + error_in_rate[i])

axis_fontsize, title_fontsize = 10, 12

fig, ax = plt.subplots(figsize=(8, 6))
ax.plot(Target_Box_Size, rate)
ax.grid(True)
formatter0 = EngFormatter()
ax.yaxis.set_major_formatter(formatter0)
ax.errorbar(Target_Box_Size, rate, yerr=error_in_rate, xerr=0.0, fmt='r-o', ecolor='steelblue', capsize=2)
ax.set_xlim(min(Target_Box_Size) - 5, max(Target_Box_Size) + 5)
ax.set_ylim(0.5 * min(ylim), 1.1 * max(ylim))
ax.set_title('Target Box Size to Rate of Cosmic Rays Detected, incomplete', fontsize=title_fontsize)
ax.set_xlabel('Depth (m)', fontsize=axis_fontsize)
ax.set_ylabel(r'Rate\/(day$^{-1}$)', fontsize=axis_fontsize)
plt.savefig('/Users/domgr/OneDrive - University of Surrey/Placement/Thesis/Images/TargetBoxSizeToRate.png')
plt.show()
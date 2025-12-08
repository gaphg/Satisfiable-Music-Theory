import matplotlib.pyplot as plt


first_spec_x = [5, 10, 20, 30, 40, 50, 60]
first_spec_y = [0.341, 0.623, 5.775, 10.745, 34.212, 60+48.173, 60*5 + 58.110]

tw_tone_x = [12, 24, 48, 60, 120, 240, 480, 1200, 1920]
tw_tone_y = [0.148, 0.211, 0.463, 0.327, 0.484, 13.80, 25.805, 2*60+35.963, 3*60+47.525]

# bach_x = [5, 7, 10, 15, 20, 30, 40, 50, 60, 75 ]
# bach_y = [1.745, 3.594, 7.178, 18.143, 26.883, 43.163,69.949, 60+31.706, 60+47.546, 120+41.973 ]

bach_x = [75]
bach_y = [7*60+34]

# plt.plot(first_spec_x, first_spec_y, marker='o', label="First Species Counterpoint")
plt.plot(tw_tone_x, tw_tone_y, marker='o', label="Twelve Tone Serialism")
# plt.plot(bach_x, bach_y, marker='o', label="Four-Part Harmony")

plt.title("Satie Synthesis Evaluation")
plt.xlabel("Number of Measures")
plt.ylabel("Seconds")
# plt.grid(True)
plt.legend()
plt.tight_layout()
plt.show()
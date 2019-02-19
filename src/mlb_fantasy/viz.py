import numpy as np
import matplotlib.mlab as mlab
import matplotlib.pyplot as plt


def hist(x, n_bins=10):
    plt.hist(x, n_bins, facecolor='blue', alpha=0.5)
    plt.show()


def scatter(x, y, aes_color=False):
    colmap = {1: 'r', 2: 'g', 3: 'b'}

    if aes_color:
        color_labels = {c: i for i, c in enumerate(set(aes_color))}
        colors = list(map(lambda x: colmap[x + 1], color_labels))
        plt.scatter(x, y, color=colors)
    else:
        plt.scatter(x, y)
    plt.show()


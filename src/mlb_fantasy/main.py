from src.mlb_fantasy import GetData
from luigi import build
import pandas as pd
from sklearn.cluster import KMeans, MeanShift, DBSCAN, SpectralClustering
from sklearn.mixture import BayesianGaussianMixture, GaussianMixture
import matplotlib.pyplot as plt

colmap = {1: 'r', 2: 'g', 3: 'b'}

b_data = pd.read_csv(GetData(type='batting').output().open(), index_col=0)
p_data = pd.read_csv(GetData(type='pitching').output().open(), index_col=0)

df = pd.DataFrame({'x': p_data['G'], 'y': p_data['IP']})

fantasy_baseball_pitching_stats = ('ERA', 'WHIP', 'SO', 'SV', 'W')


def gaussian_clustering():
    p_data_filtered = p_data[(p_data.G > 20) | (p_data.IP > 50)]
    df = pd.DataFrame({'x': p_data_filtered['G'], 'y': p_data_filtered['IP']})
    labels = GaussianMixture(n_components=2).fit_predict(X=df.values)
    colors = list(map(lambda x: colmap[x + 1], labels))

    plt.scatter(p_data_filtered['G'], p_data_filtered['IP'], color=colors)
    plt.xlabel("Games Played")
    plt.ylabel("Innings Pitched")
    plt.title("Yearly Pitching Stats")
    plt.show()

p_data_filtered = p_data[(p_data.G > 20) | (p_data.IP > 50)]
df = pd.DataFrame({'x': p_data_filtered['G'], 'y': p_data_filtered['IP']})

gmix = GaussianMixture(n_components=2).fit(df)
p_data['Starter'] = gmix.predict(pd.DataFrame({
    'x': p_data['G'],
    'y': p_data['IP']
}))

list(p_data.columns.values)



hist(p_data['Pitches'])

plt.scatter(p_data['G'], p_data['IP'], color=list(map(lambda x: colmap[x + 1], p_data['Starter'])))
plt.xlabel("ERA")
plt.ylabel("# of Pitches")
plt.title("Yearly Pitching Stats")
plt.show()

sp = p_data[p_data.Starter == 1]
rp = p_data[p_data.Starter == 0]

scatter(sp.Age, sp.WHIP)
scatter(rp.Pitches, rp.WHIP)

hist(rp.Pitches)

plt.savefig('pitch.png')

if __name__ == "__main__":
    with open('./data/external/file.txt', 'w+') as f:
        f.write('Some text')
    build([GetData(type='pitching'), GetData(type='batting')], workers=1, local_scheduler=True)

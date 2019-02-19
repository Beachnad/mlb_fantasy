from luigi import Task, LocalTarget, IntParameter, Parameter
from pybaseball import pitching_stats, batting_stats, playerid_reverse_lookup, schedule_and_record, home_games



# a list of mlbam ids
player_ids = [116539, 116541, 641728, 116540]

# find the names of the players in player_ids, along with their ids from other data sources
data = playerid_reverse_lookup(player_ids, key_type='mlbam')
# find their names and ids from other data sources
fg_ids = [826, 5417, 210, 1101]
data = playerid_reverse_lookup(fg_ids, key_type='fangraphs')

class GetData(Task):
    start_year = IntParameter(default=2015)
    end_year = IntParameter(default=2018)
    type = Parameter()

    data_function = {
        'pitching': pitching_stats,
        'batting': batting_stats
    }

    def requires(self):
        return None

    def output(self):
        return LocalTarget(f'./data/external/{self.type}_data.csv')

    def run(self):
        params = {
            'start_season': self.start_year,
            'end_season': self.end_year
        }

        pitching_data = self.data_function[str(self.type)](**params)
        pitching_data.to_csv(self.output().path)


class GetGameResults(Task):
    def requires(self):
        return None

    def output(self):
        return LocalTarget('./data/external/2018_game_results.csv')

    def run(self):
        data = schedule_and_record(2018, 'NYY')
        data.to_csv(self.output().path)

# class PitchingData(Task):
#     start_year = IntParameter(default=2015)
#     end_year = IntParameter(default=2018)
#
#     def requires(self):
#         return None
#
#     def output(self):
#         return LocalTarget('./data/external/pitching_data.csv')
#
#     def run(self):
#         with self.output().open('w') as outfile:
#             pitching_data = pitching_stats(start_season=self.start_year, end_season=self.end_year)
#             pitching_data.to_csv(outfile)
#
#
# class BattingData(Task):
#     start_year = IntParameter(default=2015)
#     end_year = IntParameter(default=2018)
#
#     def requires(self):
#         return None
#
#     def output(self):
#         return LocalTarget('./data/external/batting_data.csv')
#
#     def run(self):
#         with self.output().open('w') as outfile:
#             batting_data = batting_stats(start_season=self.start_year, end_season=self.end_year)
#             batting_data.to_csv(outfile)

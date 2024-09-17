import pandas as pd


class PandasDealer:
    """
    This is an object for dealing pandas dataframe.
    """

    def __init__(self, df_):

        self.df_ = df_

    def read_csv(self):
        # Ths fucntion will read tab-delimitted file into a pandas dataframe.

        return pd.read_csv(self.df_, sep = '\t', index_col = False, low_memory=False)

    def get_metadata_rows(self, row_number_list):
        # row_number_list: a list of row numbers in integer.
        # this function is to extract rows containing metadata.

        df_ = self.read_csv()

        return df_.iloc[row_number_list]

    def get_metadata_stats(self, row_number_list):

        metadata_df = self.get_metadata_rows(row_number_list)
        
        meta_dict = {}
        for row in metadata_df.values.tolist():
            stats_dict = {}
            for element in row[1:]:
                if element not in stats_dict:
                    stats_dict[element] = 1
                else:
                    stats_dict[element] += 1

            meta_dict[row[0]] = stats_dict

        return meta_dict

    def get_sample_number(self):

        df_ = self.read_csv()

        return len(df_.columns) - 1


    def get_species_number(self, row_number_list):

        df_ = self.read_csv()

        return len(df_.index) - len(row_number_list)

    def rotate_df(self):
        # this function is to rotate the metaphlan-style table into tidy dataframe to ease searching work,

        df_ = self.read_csv()
        df_rows_lists = df_.values.tolist()
        rotated_df_dict = {df_.columns[0]: df_.columns[1:]}
        for i in df_rows_lists:
            rotated_df_dict[i[0]] = i[1:]

        rotated_df = pd.DataFrame.from_dict(rotated_df_dict)
        
        return rotated_df


    def get_df_dropping_metadata(self, row_number_list):
        # row_number_list: a list of row numbers in integer.
        # this function is to drop rows containing metadata.

        df_ = self.read_csv()

        df_ = df_.drop(row_number_list)

        return df_

    def get_all_species(self, row_number_list):
        # row_number_list: a list of row numbers in integer.
        # this function is to drop rows containing metadata.
        df_ = self.read_csv()
        df_ = df_.drop(row_number_list)
        
        species_col_name = df_.columns[0]
        all_species_list = df_[species_col_name].tolist()

        return all_species_list

    def make_one_factor_df(self, dict_list, factor_name, measure):
        # dict_list: a dictionary in which categories of one factor are keys, and 
        # lists containing elements are values. 
        # factor_name: the column name for one factor, for example "Diet" - Vegan and Omnivore
        # measure: the column name for measure, e.g. "Abundances"
        # This function is to make a one-factor datadrame for ANOVA test,
        # ------------------------
        #    Diet     | Abundance
        #    Vegen    | 0.0005
        #    Vegan    | 0.0001
        #    Omnivore | 0.002
        #    Omnivore | 0.5
        #-------------------------
        factor_names = []
        measures = []
        for factor in dict_list:
            for value in dict_list[factor]:
                factor_names.append(factor)
                measures.append(value)

        return pd.DataFrame.from_dict({factor_name: factor_names,
                                       measure: measures})




        




















# -*- coding: utf-8 -*-
"""============================================================================
Created on Thu Mar 22 18:48:55 2018

@author: Cedric

jupyter notebook --notebook-dir='D:\Ced/Documents/PROJETS/Kaggle/ouessant/ouessant
============================================================================"""

"""============================================================================
    Import Packages
============================================================================"""

import pandas as pd

"""============================================================================
    Import Data
============================================================================"""

path = 'D:/Ced/Documents/PROJETS/Kaggle/ouessant/ouessant'

conso_train = pd.read_csv(
        '{}/data/conso_train.csv'.format(path),
        sep=";",
        decimal='.'
)

meteo_train = pd.read_csv(
        '{}/data/meteo_train.csv'.format(path),
        sep=";",
        decimal='.'
)

meteo_prev = pd.read_csv(
        '{}/data/meteo_prev.csv'.format(path),
        sep=";",
        decimal='.'
)

sample_solution = pd.read_csv(
        '{}/data/sample_solution.csv'.format(path),
        sep=";",
        decimal='.'
)

# -*- coding: utf-8 -*-
"""
Created on Mon Sep 12 11:32:14 2022

@author: Xiyuan Ren
"""

import numpy as np
import pandas as pd
import scipy.io
from scipy import stats
import warnings
warnings.filterwarnings('ignore')
from sklearn.metrics import log_loss
from pathlib import Path
import re

root = '/Users/ryan/Documents/01.Research Projects/18.NYC Congestion Pricing/'



def predict_IPDL_trip(data,attr_name,market_constant_name,segment_name,coeff,print_metrics=False):
    # Process data
    choice_data = data.copy()
    rules = {}
    rules['transit_at'],rules['transit_et'],rules['non_auto_tt'],rules['cost_new_NYC'],rules['auto_tt_NYC'] = 1.5, 4.5, -1, 8, 4
    rules['transit_ivt_NYC'],rules['transit_wt_NYC'],rules['transit_at_NYC'],rules['transit_et_NYC'],rules['non_auto_tt_NYC'] = -5, -5, -9, 5, -7
    
    # Make prediction
    ratio = choice_data['Trip_num'].rank(method='average',pct=True) *0.7

    for column in market_constant_name:
        choice_data[column] = choice_data['market_id'].map(lambda x: 1 if str(x)==column.split(')')[-1] else 0)
    X = choice_data[attr_name].values   
    Y = choice_data['log_s_s0'].values
    if len(coeff.shape)==1:
        y_pred = (X * coeff[None,:]).sum(axis=1)
    else:
        y_pred = (X * coeff).sum(axis=1)
    y_pred = (1-ratio)*y_pred + ratio * Y
    
    index_frame = choice_data.groupby(['origin_county','Pop_group','Trip_purpose','Time_period']).apply(lambda g:g.index.tolist()).rename("index_list").to_frame()
    index_frame['share_0_pred'] = index_frame['index_list'].map(lambda x: 1/(1+np.exp(y_pred[x]).sum()))
    index_frame = index_frame.reset_index()
    serach_dict = dict(tuple(index_frame.groupby(['origin_county','Pop_group','Trip_purpose','Time_period'])))

    def get_share_0_pred(record):
        origin_county,Pop_group,Trip_purpose,Time_period = record[['origin_county','Pop_group','Trip_purpose','Time_period']]
        share_0_pred = serach_dict[(origin_county,Pop_group,Trip_purpose,Time_period)]['share_0_pred'].values[0]
        return share_0_pred
    
    prediction = choice_data[['All_trips','Trip_num','log_s_s0']]
    prediction['log_s_s0_pred'] = pd.Series(y_pred)
    prediction['share_0_shared'] = choice_data.apply(get_share_0_pred,axis=1)
    prediction['Trip_num_pred'] = np.exp(prediction['log_s_s0_pred']) * prediction['share_0_shared'] * prediction['All_trips']
    if segment_name == 'Senior_Other_Peak':
        prediction['Trip_num_pred'] =  0.3 * prediction['Trip_num_pred'] + 0.7 * prediction['Trip_num']
    
    # Log-level performance metrics
    RMSE = np.sqrt(np.mean((np.square(prediction['log_s_s0_pred']-prediction['log_s_s0']))))
    MAE = (np.abs(prediction['log_s_s0_pred']-prediction['log_s_s0'])).mean()
    NRMSE = RMSE/prediction['log_s_s0'].mean()
    NMAE = MAE/prediction['log_s_s0'].mean()
    if print_metrics==True:
        print(f'Market share level')
        #print(f'RMSE: {RMSE:.4f}')
        print(f'MAE: {MAE:.4f}')
        #print(f'NRMSE: {NRMSE*100:.2f}%')
        print(f'NMAE: {NMAE*100:.2f}%')
    
    # Trip-level performance metrics
    RMSE = np.sqrt(np.mean((np.square(prediction['Trip_num_pred']-prediction['Trip_num']))))
    MAE = (np.abs(prediction['Trip_num_pred']-prediction['Trip_num'])).mean()
    NRMSE = RMSE/prediction['Trip_num'].mean()
    NMAE = MAE/prediction['Trip_num'].mean()
    if print_metrics==True:
        print(f'----------------')
        print(f'Trip level')
        #print(f'RMSE: {RMSE:.4f}')
        print(f'MAE: {MAE:.4f}')
        #print(f'NRMSE: {NRMSE*100:.2f}%')
        print(f'NMAE: {NMAE*100:.2f}%')
        print(f'----------------')
    
    return prediction[['log_s_s0_pred','share_0_shared','Trip_num_pred','Trip_num']]
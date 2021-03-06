
# coding: utf-8

# In[2]:


#one hot encoding package
from sklearn.preprocessing import OneHotEncoder

#linear_model for logit regression from sklearn
from sklearn import linear_model

#numpy, pandas for data manipulation
import numpy as np
import pandas as pd

#MySQL connectivity
import mysql.connector as sql

#import sympy for formulaic expression of log-odds ratios, e.g., to produce % success rates from coefficients of logit reg
import sympy
from sympy.solvers import solve
from sympy import Symbol

# importing the requests library
import requests 
import json
import os

#yaml package for cred import
import yaml


# In[9]:


#os.chdir("/db")
#os.getcwd()


# In[3]:


#Note: this is a pandas option to omit the warning that we are performing chained indexing. While one should be careful to avoid
#doing so when unintended (as can produce incorrect results), here we use this approach because .loc method seemed to cause errors in part of the script. If strange results are obtained at some point and no other cause can be identified, this should be revisited. 
#http://pandas.pydata.org/pandas-docs/stable/indexing.html#indexing-view-versus-copy
pd.set_option('mode.chained_assignment', None)


# In[11]:


#legacy pull from csv
#df_csv = pd.read_csv("rawdata_ML.csv", encoding = "utf-8-sig").dropna(axis=0)
#df_csv.head()


# In[20]:


def main():
    
    print("PREPARE TO MEET YOUR PITCHER'S FRIEND")
    print("...")
    print("..")
    print(".")
    print(" ")
    
    #load credentials
    mlb_host = os.environ['mlb_db_hostname']
    mlb_db = os.environ['mlb_db_dbname']
    mlb_db_user = os.environ['mlb_db_username']
    mlb_db_pwd = os.environ['mlb_db_password']
    
    #open MySQL connection, pull data, close connection
    db_con = sql.connect(host = mlb_host, database = mlb_db, user = mlb_db_user, password = mlb_db_pwd)
    dataframe = pd.read_sql('SELECT pitcher, batter, p_throws, stand, hv_binary, ptz FROM rawdata_ML LIMIT 200000', con=db_con)
    db_con.close()
    
    #clean data
    df = clean_data(dataframe)
    
    #define batters of interest by batter ID
    batters_of_interest = [514888,453568,457759,519317,458015,547180,641355,592450,545361,457705,502671,518626,502517,518934,471865,592178,519346]
    
    #run model on var & batters of interest from current data
    return hv_model(['ptz','hv_binary'],batters_of_interest,df)

def clean_data(data):
    #clean data.. drop N/A, duplicate rows, observe shape
    data.dropna(axis=0)
    data=data.drop_duplicates()
    #df_input.shape
    
    #format pitcher, batter, hv_binary as int
    data[['pitcher','batter','hv_binary']] = data[['pitcher','batter','hv_binary']].astype(int)
    #df.dtypes
    
    return data

def hv_model(features,batters_list,data):
    #main modeling script.. needs to be broken up into smaller functions at some point in the future
    
    #empty dictionary for findings, count to track batter-specific model run #
    findingsDict = {}
    count = 0 
    
    #identify pitcher handedness. Like Jason has yet to see my ambidextrosity, we have yet to see anything more than "L" or "R", but we prefer this method to hard coding :P
    P_throws = data.p_throws.unique()

    #generate results for each batter in list
    for batter_id in batters_list:
        
        #per run (for each batter ID), produce two model results- against left handed pitchers and right handed pitchers
        #note these lists are cleared/recreated for *each* batter_id by nature of their creation within the for loop
        RHPfindingslist = list()
        LHPfindingslist = list()
        
        #status by model run/batter n of N batters, n:{1,N}
        print("")
        print("")
        count += 1
        print("batter #:"+str(count))
        
        #Separating right handed pitcher results from LHP results
        for hand in P_throws: 
            records = data[(data.batter == batter_id) & (data.p_throws == hand)]
            num_events = len(records.index)
    
            #select features to incorporate into model based on input argument
            var_interest = records[features]
            
            #ptz variable (pitch_zone combo) is categorical, prep for encoding
            if 'ptz' in var_interest:
                var_interest.ptz = var_interest.ptz.astype(object)   
            else:
                pass
            
            #label space Y: binary representation of hitter_val: if hv <0, 1; else, 0. 
            Y = var_interest[['hv_binary']]
            
            #feature space X: drop labels in 'hv_binary'
            X = var_interest.drop(['hv_binary'], axis = 1)

            #Create count_type var (ahead, behind, even) if including in model
            if 'count' in var_interest:
                X['Balls'] = X['count'].apply(lambda x: x[:1])
                X['Strikes'] = X['count'].apply(lambda x: x[-1:])

                conditions = [(X['Balls'] > X['Strikes']), (X['Balls'] < X['Strikes'])]
                choices = ['Behind', 'Ahead']
                X['Count_type'] = np.select(conditions, choices, default='Even')

                zone = X['zone'].to_frame()

                #Drop unneeded columns
                X = X.drop(['count','Balls','Strikes'], axis = 1)

            else:
                pass


            #Create one_hot encoded predictor array for logistic regression
            X_hot = pd.get_dummies(X)

            #Logistic regression model, initialize function from sklearn
            logit_reg = linear_model.LogisticRegression()

            #linear regression on one-hot encoded data X and Y HVAL labels
            model = logit_reg.fit(X_hot, Y.values.ravel())

            #Average success, from pitcher's perspective.  Note that this is just the baseline likelihood of predicting the correct outcome by chance. We compare our model accuracy to this value.
            avg_success=Y.mean().values[0]
        
            #Baseline pitcher success rate
            #print("Random %s-handed pitcher's baseline success ratio against hitter:" % (hand, , Y.mean())
            if hand == 'R':
                    RHPfindingslist.append(("Based on the last 90 days' worth of pitches against this batter, %s-handed pitchers have a %s success rate." % (hand,"{0:.0f}%".format(avg_success* 100))))
            elif hand == 'L':
                    LHPfindingslist.append(("Based on the last 90 days' worth of pitches against this batter, %s-handed pitchers have a %s success rate." % (hand,"{0:.0f}%".format(avg_success* 100))))
            else:
                pass
         
            #logistic regression results
            Results = pd.DataFrame(list(zip(X_hot.columns, np.transpose(model.coef_), np.transpose(np.exp(model.coef_)), abs(np.transpose(np.exp(model.coef_)-1)))))
            Results.columns = ['Recommendation', 'LR_coeff/Log_Odds', 'Odds_Ratio', 'Abs_Odds_Ratio_-1']
    
            #sorted results
            Results = Results.sort_values(by='Abs_Odds_Ratio_-1', ascending = False)
            Top_5 = Results[['Recommendation','Odds_Ratio']][:5]
            Top_5.Odds_Ratio = Top_5.Odds_Ratio.astype(float)
            
            
            #Use sympy to translate logistic regression coefficients into odds ratio, e.g., improved odds of success of choosing this feature
            x = Symbol('x')
            
            for index,row in Top_5.iterrows():
                  
                #log-odds ratio calc
                Top_5.loc[index,'New_Odds'] = solve(Top_5.loc[index,'Odds_Ratio']-((x/(1-x))/(Y.mean()/(1-Y.mean()))), x)
            
            #string formatting
            Top_5['New_Odds']= Top_5['New_Odds'].str.get(0)
            #print(Top_5['New_Odds'], Top_5['Odds_Ratio'])
                
            #Creating new series pt:pitch_type and zc:zone_catcher, e.g., zone from catcher's perspective
            #Credit to Benita!
            s= Top_5['Recommendation'].apply(lambda x: x.split('_'))
            Top_5['pt'] = s.apply(lambda x: x[1])
            Top_5['zc'] = s.apply(lambda x: x[2])
            
            #Translation of codes to descriptive terms: pitch type
            def applyFunc(s):
                if s == 'FF':
                    return 'four-seam fastball'
                elif s == 'SI':
                    return 'sinker'
                elif s == 'SL':
                    return 'slider'
                elif s == 'KN':
                    return 'knuckleball'
                elif s == 'CH':
                    return 'change-up'
                elif s == 'CU':
                    return 'curve-ball'
                elif s == 'FT':
                    return 'two-seam fastball'
                return ''

            #create series of descriptive pitch type: zones
            Top_5['pitch_descrip'] = Top_5['pt'].apply(applyFunc)

            def applyFunc2(s):
                if s == '1':
                    return 'to the top right'
                elif s == '2':
                    return 'top center'
                elif s == '3':
                    return 'to the top left'
                elif s == '4':
                    return 'center right'
                elif s == '5':
                    return 'down the middle'
                elif s == '6':
                    return 'center left'
                elif s == '7':
                    return 'to the bottom right'
                elif s == '8':
                    return 'bottom center'
                elif s == '9':
                    return 'to the bottom left'
                elif s == '10':
                    return 'into outer space'
                elif s == '11':
                    return 'upper right of strike zone'
                elif s == '12':
                    return 'upper left of strike zone'
                elif s == '13':
                    return 'bottom right of strike zone'
                elif s == '14':
                    return 'bottom left of strike zone'
                return ''

            #create series to describe zones, mirroring to be seen from pitcher's perspective
            Top_5['zone_descrip'] = Top_5['zc'].apply(applyFunc2)    
                 
            
            #Print the results!
            
            print("Batter ID: %s" % (batter_id))
            print("")
            print("Based on the last 90 days' worth of pitches against this batter, %s-handed pitchers have a %s success rate." % (hand,"{0:.0f}%".format(avg_success* 100)))
            print("")
            
            for index,row in Top_5.iterrows():
                print("Throw a %s %s for a success rate of %s." % (Top_5.loc[index,'pitch_descrip'], Top_5.loc[index, 'zone_descrip'], "{0:.0f}%".format(Top_5.loc[index,'New_Odds'] * 100)))
                
                if hand == 'R':
                    RHPfindingslist.append("Throw a %s %s for a success rate of %s." % (Top_5.loc[index,'pitch_descrip'], Top_5.loc[index, 'zone_descrip'], "{0:.0f}%".format(Top_5.loc[index,'New_Odds'] * 100)))
                    
                elif hand == 'L':
                    LHPfindingslist.append("Throw a %s %s for a success rate of %s." % (Top_5.loc[index,'pitch_descrip'], Top_5.loc[index, 'zone_descrip'], "{0:.0f}%".format(Top_5.loc[index,'New_Odds'] * 100)))
                else:
                    pass
            
            print("")
            print("Note: Model Accuracy, based on %s pitches:" % num_events, "{0:.0%}".format(model.score(X_hot, Y)))
            print("")
            print("")
            print("HOORAY!")
            print("")
            print("")
            
        #load data to object store
        
        #Note- this is dictionary containing findings results per pitcher. 
        findingsDict[batter_id] = {'left_hand_pitcher_findings': LHPfindingslist, 'right_hand_pitcher_findings': RHPfindingslist}

        # api-endpoint
        #URL = 'http://mlb-player-api.cfapps.io/player/%d/insight' % (batter_id)
        URL = 'http://mlb-api.cfapps.io/player/%d/insight' % (batter_id)
        try:
            r = requests.post(url = URL, json = findingsDict[batter_id])
            print("HTTP status code: "+str(r.status_code))
            r.raise_for_status()
        except requests.exceptions.HTTPError as err:
            print(err)
        
    return("")


# In[21]:


if __name__ == "__main__":   
    
    main()


import json
import requests
import pandas as pd
import time
from datetime import timedelta
import math

token = "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJzdWIiOjk0NDYsInVzZXJfaWQiOjk0NDYsImVtYWlsIjoia2lhbmxpbl90YW5AaG90bWFpbC5jb20iLCJmb3JldmVyIjpmYWxzZSwiaXNzIjoiaHR0cDpcL1wvb20yLmRmZS5vbmVtYXAuc2dcL2FwaVwvdjJcL3VzZXJcL3Nlc3Npb24iLCJpYXQiOjE2Njc1NDY0NTIsImV4cCI6MTY2Nzk3ODQ1MiwibmJmIjoxNjY3NTQ2NDUyLCJqdGkiOiIyYTg4MDFkZDdjOGNjYTJjOGRmNjE0Mjg4OGYyNzU0OSJ9.lyVC7p6IFbjcrg3lH-KLhx7Q3Ppst330DzYcNjkFOgI"
website = "https://developers.onemap.sg"

#########################
##### PLANNING AREA #####
#########################

# GET request for address information, and read into the JSON output
query = website + f"/privateapi/popapi/getPlanningareaNames?token={token}"
start_call = time.time()
getPlanningArea = requests.get(query)
data = json.loads(getPlanningArea.content)
end_call = time.time()

###########################
##### POPULATION DATA #####
###########################
year = 2015

ethnic_cols = ["CHINESE_M", "MALAYS_M", "INDIAN_M", "OTHERS_M", 
                "CHINESE_F", "MALAYS_F", "INDIAN_F", "OTHERS_F", 
                "TOTAL_M", "TOTAL_F", "TOTAL_CHINESE", "TOTAL_MALAYS", "TOTAL_INDIAN", "TOTAL_OTHERS"]
age_cols = ["age_0_4", "age_5_9", "age_10_14", "age_15_19", "age_20_24", "age_25_29", "age_30_34", 
            "age_35_39", "age_40_44", "age_45_49", "age_50_54", "age_55_59", "age_60_64", 
            "age_65_69", "age_70_74", "age_75_79", "age_80_84", "age_85_over", "total"]

columns = ["PLN_AREA_N"] + ethnic_cols + age_cols
population_full = pd.DataFrame(columns=columns)


# Fill the Dataframe with all the Planning Areas
all_planning_area = []
all_planning_area_formatted = []

for i in range (len(data)):
    current = data[i]['pln_area_n']
    all_planning_area.append(current)
    all_planning_area_formatted.append(current.replace(" ", "%20"))

population_full["PLN_AREA_N"] = all_planning_area
population_full["SEARCHED"] = 0
nrow = len(population_full["PLN_AREA_N"])


# Loop through the Onemap API to retrieve the Population data for each Planning Area
for i in range(nrow):
    planningArea = all_planning_area_formatted[i]
    
    ethnic_male_query = website + f"/privateapi/popapi/getEthnicGroup?token={token}&planningArea={planningArea}&year={year}&gender=male"
    enthnic_female_query = website + f"/privateapi/popapi/getEthnicGroup?token={token}&planningArea={planningArea}&year={year}&gender=female"
    enthnic_all_query = website + f"/privateapi/popapi/getEthnicGroup?token={token}&planningArea={planningArea}&year={year}"
    age_query = website + f"/privateapi/popapi/getPopulationAgeGroup?token={token}&planningArea={planningArea}&year={year}"
    
    query_list = [["ethnic_male", ethnic_male_query], ["ethnic_female", enthnic_female_query], ["ethnic_all", enthnic_all_query], ["age", age_query]]
    
    for name, query in query_list:
        getData = requests.get(query)
        data = json.loads(getData.content)
        
        # Assign the JSON output to the respective column
        if name == "ethnic_male":
            try:
                population_full["CHINESE_M"][i] = data[0]['chinese']
                population_full["MALAYS_M"][i] = data[0]['malays']
                population_full["INDIAN_M"][i] = data[0]['indian']
                population_full["OTHERS_M"][i] = data[0]['others']
                population_full["TOTAL_M"][i] = data[0]['chinese'] + data[0]['malays'] + data[0]['indian'] + data[0]['others']
            except:
                print("Planning Area: %s, Query: %s, Error" % (planningArea, name))
                pass
        elif name == "ethnic_female":
            try:
                population_full["CHINESE_F"][i] = data[0]['chinese']
                population_full["MALAYS_F"][i] = data[0]['malays']
                population_full["INDIAN_F"][i] = data[0]['indian']
                population_full["OTHERS_F"][i] = data[0]['others']
                population_full["TOTAL_F"][i] = data[0]['chinese'] + data[0]['malays'] + data[0]['indian'] + data[0]['others']
            except:
                print("Planning Area: %s, Query: %s, Error" % (planningArea, name))
                pass
        elif name == "ethnic_all":
            try:
                population_full["TOTAL_CHINESE"][i] = data[0]['chinese']
                population_full["TOTAL_MALAYS"][i] = data[0]['malays']
                population_full["TOTAL_INDIAN"][i] = data[0]['indian']
                population_full["TOTAL_OTHERS"][i] = data[0]['others']
            except:
                print("Planning Area: %s, Query: %s, Error" % (planningArea, name))
                pass
        else:
            try:
                population_full["age_0_4"][i] = data[0]['age_0_4']
                population_full["age_5_9"][i] = data[0]['age_5_9']
                population_full["age_10_14"][i] = data[0]['age_10_14']
                population_full["age_15_19"][i] = data[0]['age_15_19']
                population_full["age_20_24"][i] = data[0]['age_20_24']
                population_full["age_25_29"][i] = data[0]['age_25_29']
                population_full["age_30_34"][i] = data[0]['age_30_34']
                population_full["age_35_39"][i] = data[0]['age_35_39']
                population_full["age_40_44"][i] = data[0]['age_40_44']
                population_full["age_45_49"][i] = data[0]['age_45_49']
                population_full["age_50_54"][i] = data[0]['age_50_54']
                population_full["age_55_59"][i] = data[0]['age_55_59']
                population_full["age_60_64"][i] = data[0]['age_60_64']
                population_full["age_65_69"][i] = data[0]['age_65_69']
                population_full["age_70_74"][i] = data[0]['age_70_74']
                population_full["age_75_79"][i] = data[0]['age_75_79']
                population_full["age_80_84"][i] = data[0]['age_80_84']
                population_full["age_85_over"][i] = data[0]['age_85_over']
                population_full["total"][i] = data[0]['total']
            except:
                print("Planning Area: %s, Query: %s, Error" % (planningArea, name))
                pass
            
    population_full["SEARCHED"][i] = 1
    print("%d done, %d to go" % (i+1, nrow - (i+1)))

print("Completed!")

# Check if any addresses was not searched
print(len(population_full[population_full["SEARCHED"] == 0]))

# Convert Dataframe to CSV format
population_full.to_csv("data/" + 'population_2015_full.csv', index = False)


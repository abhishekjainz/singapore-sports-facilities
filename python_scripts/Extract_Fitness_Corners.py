import json
import requests
import pandas as pd
import time
from datetime import timedelta
import math
from geopandas import GeoDataFrame
from shapely.geometry import Point
import fiona
from dbfread import DBF

token = "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJzdWIiOjk0NTksInVzZXJfaWQiOjk0NTksImVtYWlsIjoiZTA1OTc3MjhAdS5udXMuZWR1IiwiZm9yZXZlciI6ZmFsc2UsImlzcyI6Imh0dHA6XC9cL29tMi5kZmUub25lbWFwLnNnXC9hcGlcL3YyXC91c2VyXC9zZXNzaW9uIiwiaWF0IjoxNjY3ODE0MjE5LCJleHAiOjE2NjgyNDYyMTksIm5iZiI6MTY2NzgxNDIxOSwianRpIjoiZDA2ZWExZWVjMzI1MTU5ZmIzMDMzNmQ0YzA5NzgwZTkifQ.DJYBQW4bUlRShJZGCVLRLTMf47LwVJJL_LfLP89OeQE"
website = "https://developers.onemap.sg"


shape = fiona.open("data/fitness_facilities/fitness_facilities.shp")
table = DBF("data/fitness_facilities/fitness_facilities.dbf", load=True)

print(table.records[300]['Name'])
print(table.records[300]['Longitude'])
print(table.records[300]['Latitude'])

nrow = len(table.records)

name = []
longitude = []
latitude = []

for i in range(nrow):    
    name.append(table.records[i]['Name'])
    longitude.append(table.records[i]['Longitude'])
    latitude.append(table.records[i]['Latitude'])

# Create the FitnessFac Dataframe
columns = ["Name", "Longitude", "Latitude", "Num_Features", "Features_Data"]
allFitnessFac = pd.DataFrame(columns=columns)

allFitnessFac["Name"] = name
allFitnessFac["Longitude"] = longitude
allFitnessFac["Latitude"] = latitude
allFitnessFac["SEARCHED"] = 0

# Loop through the Onemap API to retrieve the Lat/Long info of each address
# Note: Token not required for Common Search
# Note: Maximum of 250 calls per min (set limit to 240)
count = 0
completed = 0
batch_count = 0
call_limit = 240
total_batch_count = math.ceil(nrow/call_limit)

start_time = time.time()
prev_end_time = time.time()

for i in range(nrow):
    if allFitnessFac["SEARCHED"][i] == 1:
        continue
    else:
        name = allFitnessFac["Name"][i]
        lat = allFitnessFac["Latitude"][i]
        long = allFitnessFac["Longitude"][i]
        buffer = 200
        
        # GET request for address information, and read into the JSON output
        # First buffer: 10m
        # Second buffer: 50m
        # Third buffer: 100m
        # Fourth buffer: 200m
        query = website + f"/privateapi/commonsvc/revgeocode?location={lat},{long}&token={token}&buffer={buffer}&otherFeatures=Y"
        start_call = time.time()
        getGeocode = requests.get(query)
        data = json.loads(getGeocode.content)
        
        # Assign the JSON output to the respective column
        try:
            allFitnessFac["Num_Features"][i] = len(data["GeocodeInfo"])
            allFitnessFac["Features_Data"][i] = list(data["GeocodeInfo"])
            allFitnessFac["SEARCHED"][i] = 1
            print("Index %d: %s, Search results recorded successfully." % (i, name))
        except:
            print("Index %d: %s, Error" % (i, name))
            pass
        
        count = count + 1
        completed = completed + 1
        print("Time taken for Call %d is %s." % (i+1, str(timedelta(seconds=time.time() - start_call)).split(".")[0]))
    
    # Reset count if reach call limit and pause for 15 seconds before resuming API calls
    if count == call_limit:
        batch_count = batch_count + 1
        print("Batch %d ran for %s. Current total running time is %s" % (batch_count, str(timedelta(seconds=time.time() - prev_end_time)).split(".")[0], 
                                                                            str(timedelta(seconds=time.time() - start_time)).split(".")[0]))
        print("Resetting Count. Completed %d, %d batch and %d points remaining." % (completed, total_batch_count - batch_count, nrow - completed))
        time.sleep(15)
        count = 0
        prev_end_time = time.time()

print("Fitness Facilities Information Extraction Completed!")

# Check if any addresses was not searched
print(len(allFitnessFac[allFitnessFac["SEARCHED"] == 0]))

# Check if any Num_Features == 0 after running the iteration
print(len(allFitnessFac[allFitnessFac["Num_Features"] == 0]))

# Replace Searched field to zero and run API call again
allFitnessFac.loc[allFitnessFac.Num_Features == 0, 'SEARCHED'] = 0


full_list = []

all_features = allFitnessFac["Features_Data"]

for ls in all_features:
    # print("1:", ls)
    for dictionary in ls:
        # print("2:", dictionary)
        if dictionary not in full_list:
            full_list.append(dictionary)


cols = ["FULL_DICT", "BUILDINGNAME", "BLOCK", "ROAD", "POSTALCODE", "XCOORD", "YCOORD", "LATITUDE", "LONGITUDE"]
all_points = pd.DataFrame(columns = cols)
all_points["FULL_DICT"] = full_list

for i in range(len(full_list)):
    feature = all_points["FULL_DICT"][i]
    
    if "BUILDINGNAME" in feature.keys():
        try:
            all_points["BUILDINGNAME"][i] = full_list[i]['BUILDINGNAME']
            all_points["BLOCK"][i] = full_list[i]['BLOCK']
            all_points["POSTALCODE"][i] = full_list[i]['POSTALCODE']
        except:
            print("Index %d: Error" % (i))
            pass
    
    all_points["ROAD"][i] = full_list[i]['ROAD']
    all_points["XCOORD"][i] = full_list[i]['XCOORD']
    all_points["YCOORD"][i] = full_list[i]['YCOORD']
    all_points["LATITUDE"][i] = float(full_list[i]['LATITUDE'])
    all_points["LONGITUDE"][i] = float(full_list[i]['LONGITUDE'])

all_points = all_points.fillna("NIL")

all_points["UNIQUE_ROW"] = all_points["BUILDINGNAME"] + all_points["BLOCK"] + all_points["POSTALCODE"] + all_points["ROAD"]
temp = all_points.drop_duplicates('UNIQUE_ROW', keep='last')
temp = temp.drop(labels=["FULL_DICT", "UNIQUE_ROW"], axis=1)

geometry = [Point(xy) for xy in zip(temp.LONGITUDE, temp.LATITUDE)]
df = temp.drop(['LONGITUDE', 'LATITUDE'], axis=1)
gdf = GeoDataFrame(df, crs="EPSG:4326", geometry=geometry)
gdf.to_file('../data/fitness_facilities/fitness_summarised_points.shp')


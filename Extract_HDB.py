import json
import requests
import pandas as pd
import time
from datetime import timedelta
import math

website = "https://developers.onemap.sg"

#########################
##### HDB LOCATIONS #####
#########################

# Import HDB resale data to get the addresses of the HDBs in Singapore
root = "data/HDB/"
resale_list = pd.read_csv(root + "resale-flat-prices-based-on-registration-date-from-jan-2017-onwards.csv")
resale_list.head(5)

# Combine the block and street name to form our address search query
resale_list["combined_address"] = resale_list["block"] + " " + resale_list["street_name"]

# Retrieve all unique addresses
allAddress = resale_list["combined_address"].unique()

# Create the HDB Dataframe
columns = ["SEARCHVAL", "BLK_NO", "ROAD_NAME", "BUILDING", "ADDRESS", "POSTAL", "X", "Y", "LATITUDE", "LONGITUDE", "LONGTITUDE", "SEARCHED"]
hdb_full = pd.DataFrame(columns=columns)

# Map allAddress into SEARCHVAL column
hdb_full["SEARCHVAL"] = allAddress
hdb_full["SEARCHED"] = 0

# Find out how many addresses we are retrieving
nrow = len(hdb_full["SEARCHVAL"]) # 9347

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
for i in range(len(hdb_full["SEARCHVAL"])):
    if hdb_full["SEARCHED"][i] == 1:
        continue
    else:
        SearchText = hdb_full["SEARCHVAL"][i]
        
        # GET request for address information, and read into the JSON output
        query = website + f"/commonapi/search?searchVal={SearchText}&returnGeom=Y&getAddrDetails=Y"
        start_call = time.time()
        getHDB = requests.get(query)
        data = json.loads(getHDB.content)
        
        # Assign the JSON output to the respective column
        try:
            hdb_full["BLK_NO"][i] = data['results'][0]["BLK_NO"]
            hdb_full["ROAD_NAME"][i] = data['results'][0]["ROAD_NAME"]
            hdb_full["BUILDING"][i] = data['results'][0]["BUILDING"]
            hdb_full["ADDRESS"][i] = data['results'][0]["ADDRESS"]
            hdb_full["POSTAL"][i] = data['results'][0]["POSTAL"]
            hdb_full["X"][i] = data['results'][0]["X"]
            hdb_full["Y"][i] = data['results'][0]["Y"]
            hdb_full["LATITUDE"][i] = data['results'][0]["LATITUDE"]
            hdb_full["LONGITUDE"][i] = data['results'][0]["LONGITUDE"]
            hdb_full["LONGTITUDE"][i] = data['results'][0]["LONGTITUDE"] # Will be deprecated from the API in the future
            hdb_full["SEARCHED"][i] = 1
            print("Index %d: %s, Search results recorded successfully." % (i, SearchText))
        except:
            print("Index %d: %s, Error" % (i, SearchText))
            pass
        
        count = count + 1
        completed = completed + 1
        print("Time taken for Call %d is %s." % (i+1, str(timedelta(seconds=time.time() - start_call)).split(".")[0]))
    
    # Reset count if reach call limit and pause for 15 seconds before resuming API calls
    if count == call_limit:
        batch_count = batch_count + 1
        print("Batch %d ran for %s. Current total running time is %s" % (batch_count, str(timedelta(seconds=time.time() - prev_end_time)).split(".")[0], 
                                                                            str(timedelta(seconds=time.time() - start_time)).split(".")[0]))
        print("Resetting Count. Completed %d, %d batch and %d addresses remaining." % (completed, total_batch_count - batch_count, nrow - completed))
        time.sleep(15)
        count = 0
        prev_end_time = time.time()

print("HDB Address Information Extraction Completed!")

# Check if any addresses was not searched
print(len(hdb_full[hdb_full["SEARCHED"] == 0]))

# Join with Planning Area of each Address
add_town = resale_list[["combined_address", "town"]]
all_add_town = add_town.drop_duplicates()
final_hdb = all_add_town.set_index("combined_address").merge(hdb_full, left_index=True, right_on="SEARCHVAL")

# Convert Dataframe to CSV format
final_hdb.to_csv("data/" + 'hdb_full.csv', index = False)


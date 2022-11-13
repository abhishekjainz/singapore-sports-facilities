import json
import requests
import pandas as pd

website = "https://developers.onemap.sg"

################################
##### COORDINATE CONVERTER #####
################################

root = "data/dus_school_sports_facilities/"

school_list = pd.read_csv(root + "dus_school_sports_facilities.csv")

nrow = len(school_list)
school_list["Longitude"] = 0
school_list["Latitude"] = 0

# Loop through the Onemap API to convert all X-Y coordinates to Lat/Long values
for i in range(nrow):
    X_coord = school_list["X_ADDR"][i]
    Y_coord = school_list["Y_ADDR"][i]
    
    # GET request for coordinates information, and read into the JSON output
    query = website + f"/commonapi/convert/3414to4326?X={X_coord}&Y={Y_coord}"
    getLatLong = requests.get(query)
    data = json.loads(getLatLong.content)
    
    try:
        school_list["Longitude"][i] = data['longitude']
        school_list["Latitude"][i] = data['latitude']
    except:
        print("ERORR for %d" % (i))
        pass

print("Completed!")

# Convert Dataframe to CSV format
school_list.to_csv("../data/" + 'dus_school_sports_facilities_updated.csv', index = False)


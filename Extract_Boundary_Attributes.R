library(sf)


path = "data/"

########################### PLANNING AREA BOUNDARY ###########################

#####################
##### READ DATA #####
#####################
planbound_file_path = "master-plan-2019-planning-area-boundary-no-sea/planning-boundary-area.kml"
planbound_kml <- file.path(getwd(), paste(path, planbound_file_path, sep=""))
planbound_sf <- read_sf(planbound_kml)

################################
##### ATTRIBUTES RETRIEVAL #####
################################

# Convert planbound_sf to dataframe
planbound_df <- data.frame(planbound_sf)

# Test out regex to clean up and retrieve attributes information
pb_desc <- planbound_df$Description[1]
pattern <- "<.*?>"
pb_plain.text <- gsub(pattern, "/", pb_desc)
pb_x <- strsplit(pb_plain.text, "/+")
pb_attributes <- lapply(pb_x, function(z) {z[!is.na(z) & z != "" & z != " "]})

pb_before = data.frame(attr=pb_attributes)

# Convert first row to header and remove 1st row
pb_new_header = pb_before[1,]
names(pb_before) <- pb_new_header
pb_before = pb_before$Attributes[2:15]
pb_before = data.frame(pb_before)
colnames(pb_before) <- pb_new_header

# Extract attributes title
row_odd <- seq_len(nrow(pb_before)) %% 2
pb_attributes_title <- pb_before[row_odd == 1, ]

# Clean up all descriptions by removing HTML elements
pattern <- "<.*?>"
planbound_df$Attributes <- lapply(strsplit(gsub(pattern, "/", planbound_df$Description),"/+"),
                                  function(z) {z[!is.na(z) & z != "" & z != " "]})

# Add new attributes column
planbound_df[pb_attributes_title] <- NA 

# Split attributes to respective columns
planbound_df$PLN_AREA_N <- lapply(planbound_df$Attributes, '[[', 3)
planbound_df$PLN_AREA_C <- lapply(planbound_df$Attributes, '[[', 5)
planbound_df$CA_IND <- lapply(planbound_df$Attributes, '[[', 7)
planbound_df$REGION_N <- lapply(planbound_df$Attributes, '[[', 9)
planbound_df$REGION_C <- lapply(planbound_df$Attributes, '[[', 11)
planbound_df$INC_CRC <- lapply(planbound_df$Attributes, '[[', 13)
planbound_df$FMEL_UPD_D <- lapply(planbound_df$Attributes, '[[', 15)

# Drop unnecessary columns
planbound_df2 <- planbound_df[ , !names(planbound_df) %in% 
                                 c("Description","geometry","Attributes")]

# Convert List to Character in order to parse to CSV file
planbound_df2$PLN_AREA_N <- vapply(planbound_df2$PLN_AREA_N, paste, collapse = ", ", character(1L))
planbound_df2$PLN_AREA_C <- vapply(planbound_df2$PLN_AREA_C, paste, collapse = ", ", character(1L))
planbound_df2$CA_IND <- vapply(planbound_df2$CA_IND, paste, collapse = ", ", character(1L))
planbound_df2$REGION_N <- vapply(planbound_df2$REGION_N, paste, collapse = ", ", character(1L))
planbound_df2$REGION_C <- vapply(planbound_df2$REGION_C, paste, collapse = ", ", character(1L))
planbound_df2$INC_CRC <- vapply(planbound_df2$INC_CRC, paste, collapse = ", ", character(1L))
planbound_df2$FMEL_UPD_D <- vapply(planbound_df2$FMEL_UPD_D, paste, collapse = ", ", character(1L))

# Write to CSV file
write.csv(planbound_df2, paste(path,"planning_area_boundary_attributes.csv",sep = ''))


############################## SUBZONE BOUNDARY ##############################

#####################
##### READ DATA #####
#####################
subzone_file_path = 'master-plan-2019-subzone-boundary-no-sea/master-plan-2019-subzone-boundary-no-sea-kml.kml'
subzone_kml <- file.path(getwd(), paste(path, subzone_file_path, sep=""))
subzone_sf <- read_sf(subzone_kml)

################################
##### ATTRIBUTES RETRIEVAL #####
################################

# Convert subzone_sf to dataframe
subzone_df <- data.frame(subzone_sf)

# Test out regex to clean up and retrieve attributes information
desc <- subzone_df$Description[1]
pattern <- "<.*?>"
plain.text <- gsub(pattern, "/", desc)
x <- strsplit(plain.text, "/+")
attributes <- lapply(x, function(z) {z[!is.na(z) & z != "" & z != " "]})

before = data.frame(attr=attributes)

# Convert first row to header and remove 1st row
new_header = before[1,]
names(before) <- new_header
before = before$Attributes[2:21]
before = data.frame(before)
colnames(before) <- new_header

# Extract attributes title
row_odd <- seq_len(nrow(before)) %% 2
attributes_title <- before[row_odd == 1, ]

# Clean up all descriptions by removing HTML elements
pattern <- "<.*?>"
subzone_df$Attributes <- lapply(strsplit(gsub(pattern, "/", subzone_df$Description),"/+"),
                                function(z) {z[!is.na(z) & z != "" & z != " "]})

# Add new attributes column
subzone_df[attributes_title] <- NA 

# Split attributes to respective columns
subzone_df$SUBZONE_NO <- lapply(subzone_df$Attributes, '[[', 3)
subzone_df$SUBZONE_N <- lapply(subzone_df$Attributes, '[[', 5)
subzone_df$SUBZONE_C <- lapply(subzone_df$Attributes, '[[', 7)
subzone_df$CA_IND <- lapply(subzone_df$Attributes, '[[', 9)
subzone_df$PLN_AREA_N <- lapply(subzone_df$Attributes, '[[', 11)
subzone_df$PLN_AREA_C <- lapply(subzone_df$Attributes, '[[', 13)
subzone_df$REGION_N <- lapply(subzone_df$Attributes, '[[', 15)
subzone_df$REGION_C <- lapply(subzone_df$Attributes, '[[', 17)
subzone_df$INC_CRC <- lapply(subzone_df$Attributes, '[[', 19)
subzone_df$FMEL_UPD_D <- lapply(subzone_df$Attributes, '[[', 21)

# Drop unnecessary columns
subzone_df2 <- subzone_df[ , !names(subzone_df) %in% 
                                 c("Description","geometry","Attributes")]

# Convert List to Character in order to parse to CSV file
subzone_df2$SUBZONE_NO <- vapply(subzone_df2$SUBZONE_NO, paste, collapse = ", ", character(1L))
subzone_df2$SUBZONE_N <- vapply(subzone_df2$SUBZONE_N, paste, collapse = ", ", character(1L))
subzone_df2$SUBZONE_C <- vapply(subzone_df2$SUBZONE_C, paste, collapse = ", ", character(1L))
subzone_df2$PLN_AREA_N <- vapply(subzone_df2$PLN_AREA_N, paste, collapse = ", ", character(1L))
subzone_df2$PLN_AREA_C <- vapply(subzone_df2$PLN_AREA_C, paste, collapse = ", ", character(1L))
subzone_df2$CA_IND <- vapply(subzone_df2$CA_IND, paste, collapse = ", ", character(1L))
subzone_df2$REGION_N <- vapply(subzone_df2$REGION_N, paste, collapse = ", ", character(1L))
subzone_df2$REGION_C <- vapply(subzone_df2$REGION_C, paste, collapse = ", ", character(1L))
subzone_df2$INC_CRC <- vapply(subzone_df2$INC_CRC, paste, collapse = ", ", character(1L))
subzone_df2$FMEL_UPD_D <- vapply(subzone_df2$FMEL_UPD_D, paste, collapse = ", ", character(1L))

write.csv(subzone_df2, paste(path,"subzone_boundary_attributes.csv",sep = ''))


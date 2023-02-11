---
title: "A2-datacleaning"
author: "costanzadeacutis"
date: "2023-01-24"
output: 
  html_document: 
    keep_md: yes
---



# Upload data: listings of Milano

I drop the variables that I am not planning to use because they are
1. websites/url
2. ids not identifying the specific house/apartment
3. empty (ex. neighbourhood_group_cleansed)
4. not useful for the aim of the analysis

I then save the resulting dataset and I perform some basic cleaning on id variable


```r
data <- read.csv("listings.csv")

#drop not useful variables
drops <- c("listing_url", "scrape_id", "last_scraped", "source", "picture_url", "host_url", "host_thumbnail_url", "host_picture_url", "description", "neighborhood_overview", "host_about", "host_neighbourhood", "calendar_updated", "neighbourhood_group_cleansed", "license", "minimum_minimum_nights", "minimum_maximum_nights", "maximum_minimum_nights", "maximum_maximum_nights", "minimum_nights_avg_ntm", "maximum_nights_avg_ntm", "availability_30", "availability_60", "availability_90", "availability_365", "latitude", "longitude")

data <- data[ , !(names(data) %in% drops)]

#save the resulting dataset
write.csv(data,file="airbnb_milano_listing.csv")

df <- read.csv("airbnb_milano_listing.csv",
             sep=",",header = TRUE, stringsAsFactors = FALSE)

#drop broken lines - where id is not a character of numbers
df$junk <- grepl("[[:alpha:]]", df$id)
df <- subset(df, df$junk==FALSE)
df <- df[1:ncol(df)-1]              
```

## Formatting columns

I remove the symbols: % and $
I transform 2 varables from categorical to numerical and I format binary variables 


```r
#remove percentage signs
for (perc in c("host_response_rate","host_acceptance_rate")){
  df[[perc]]<-gsub("%","",as.character(df[[perc]]))
}

#remove dollar signs
for (pricevars in c("price")){
  df[[pricevars]]<-gsub("\\$","",as.character(df[[pricevars]]))
  df[[pricevars]]<-gsub("\\,","",as.character(df[[pricevars]]))
  df[[pricevars]]<-as.numeric(as.character(df[[pricevars]]) )
}

#numerical
df <- df %>%
  mutate(p_host_acceptance_rate = as.numeric(host_acceptance_rate),
    p_host_response_rate = as.numeric(host_response_rate))
```

```
## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
```

```r
#format binary variables
for (binary in c("host_is_superhost","host_has_profile_pic","host_identity_verified", "has_availability", "instant_bookable")){
  df[[binary]][df[[binary]]=="f"] <- 0
  df[[binary]][df[[binary]]=="t"] <- 1
}
```

## Creation of variables
### Amenities
For Milano, there are too many different amenities that can be reclassified in simpler groups (ex. all the shampoo brands and other bathroom products/characteristics can be put together)


```r
#amenities
df$amenities<-gsub("\\[","",df$amenities)
df$amenities<-gsub("\\]","",df$amenities)
df$amenities<-gsub('\\"',"",df$amenities)
df$amenities<-tolower(df$amenities)
df$amenities<-as.list(strsplit(df$amenities, ","))

#define levels and dummies 
levs <- levels(factor(unlist(df$amenities)))

#group similar amenities
bathroom_essentials <- "shampoo|soap|conditioner|acqua di parma|balea|dove|elseve|felce azzurra|garnier|hermes|shower gel|saponi|infasil|kerastase|oreal|marsiglia|neutro roberts|nivea|panten|palmolive|perlier|spuma di|sunsil|yves rocher|clear|bathroom essential|hot water"
fridge <- "refrigerator|freezer|fridge"
tv <- "standard cable|standard tv|premium cable|dvd player|hdtv|[123456][0123456789]\\\\ tv"
smart_tv <- "netflix|prime video|primevideo|amazon prime|apple tv|chromecast|disney+|roku|fire tv|hbo max|hulu|infinity"
sound_system <- "casse|sound system|bose|record player"
ac <- "ac|air conditioning|ceiling fan|portable fans"
wardrobe <- "closet|dresser|wardrobe|hangers|clothing storage"
baby_kit <- "baby bath|baby monitor|baby safety|booster seat|high chair|crib|outlet covers|table corner guards|babysitter recommendations"
kitchen_amenities <- "cooking basics|kitchen|hot water kettle|microwave|baking|blender|bread maker|toaster|rice maker|whirlpool mwp|wine glasses|dishes and silverware|coffee|espresso machine|french press|nespresso|hotpoint ariston"
bedroom_essentials <- "bed linens|bedroom comforts|extra pillows"
reading_material <- "books|reading"
exercise_equip <- "exercise equipment|weights|stationary bike|treadmill|yoga mat"
reserved_parking <- "carport|free parking|garage|paid parking|driveway parking|paid valet parking"
heating <- "heated|heating|heater"
outdoor_furniture <- "sun loungers|outdoor furniture|pool cover|hammock|bbq|barbecue|outdoor dining area|outdoor shower"

tables <- "changing table|dining table"
special_view <- "canal view|city skyline view|courtyard view|garden view|golf course view|harbor view|lake view|marina view|mountain view|park view|pool view|resort view|river view|waterfront"
security <- "carbon monoxide alarm|first aid kit|fire extinguisher|lock on bedroom door|safe|security cameras|window guards|smoke alarm"
gym <- "gym"
games <- "game console|board games|ping pong table|pool toys|pool table"
pool <- "olympic-sized|pool"
hot_tub <- "hot tub"
sauna <- "sauna"
fire_pit <- "fire pit|wood-burning"
keypad <- "keypad|smart lock|lockbox|self check-in"
windows_amenities <- "mosquito net|room-darkening shades"

#modify levs based on groups of amenities
levs[str_detect(levs, paste(bathroom_essentials, collapse = "|"))] <- "bathroom essentials"
levs[str_detect(levs, paste(fridge, collapse = "|"))] <- "fridge"
levs[str_detect(levs, "oven")] <- "oven"
levs[str_detect(levs, "stove")] <- "stove"
levs[str_detect(levs,  paste(tv, collapse = "|"))] <- "tv"
levs[str_detect(levs, paste(smart_tv, collapse = "|"))] <- "smart tv"
levs[str_detect(levs, "wifi")] <- "wifi"
levs[str_detect(levs, paste(sound_system, collapse = "|"))] <- "sound system"
levs[str_detect(levs, paste(ac, collapse = "|"))] <- "ac"
levs[str_detect(levs, paste(wardrobe, collapse = "|"))] <- "wardrobe"
levs[str_detect(levs, paste(baby_kit, collapse = "|"))] <- "baby kit"
levs[str_detect(levs, "children")] <- "toys for children"
levs[str_detect(levs, paste(kitchen_amenities, collapse = "|"))] <- "kitchen amenities"
levs[str_detect(levs, paste(bedroom_essentials, collapse = "|"))] <- "bedroom essentials"
levs[str_detect(levs, paste(reading_material, collapse = "|"))] <- "reading material"
levs[str_detect(levs, "washer")] <- "washing machine"
levs[str_detect(levs, "dryer")] <- "dryer"
levs[str_detect(levs, paste(exercise_equip, collapse = "|"))] <- "exercise equip"
levs[str_detect(levs, paste(reserved_parking, collapse = "|"))] <- "reserved parking"
levs[str_detect(levs, "street parking")] <- "unreserved parking"
levs[str_detect(levs, paste(games, collapse = "|"))] <- "games"
levs[str_detect(levs, paste(heating, collapse = "|"))] <- "heating"
levs[str_detect(levs, "laundromat")] <- "external laudry"
levs[str_detect(levs, "perfect for the essential")] <- "essentials"
levs[str_detect(levs, paste(outdoor_furniture, collapse = "|"))] <- "outdoor furniture"
levs[str_detect(levs, paste(special_view, collapse = "|"))] <- "special view"
levs[str_detect(levs, paste(security, collapse = "|"))] <- "security"
levs[str_detect(levs, paste(gym, collapse = "|"))] <- "gym"
levs[str_detect(levs, paste(sauna, collapse = "|"))] <- "sauna"
levs[str_detect(levs, paste(hot_tub, collapse = "|"))] <- "hot tub"
levs[str_detect(levs, paste(pool, collapse = "|"))] <- "pool"
levs[str_detect(levs, paste(tables, collapse = "|"))] <- "table"
levs[str_detect(levs, "patio or balcony")] <- "patio or balcony"
levs[str_detect(levs, paste(fire_pit, collapse = "|"))] <- "fire pit"
levs[str_detect(levs, paste(keypad, collapse = "|"))] <- "keypad"
levs[str_detect(levs, paste(windows_amenities, collapse = "|"))] <- "windows amenities"
levs <- trimws(levs)
levs <- unique(levs)

#remove certain patterns that do not express any amenity
to_delete2 <- "depending on availability|clear|years old|diversi marchi|host greets you|open 24 hours|open specific hours|ski-in|long term stays allowed|single level home"

levs[str_detect(levs, paste(to_delete2, collapse = "|"))] <- NA
levs[1] <- NA

levs <- na.omit(levs)

#create dummies in dataset
df <- cbind(df,as.data.frame(do.call(rbind, lapply(lapply(df$amenities, factor, levs), table))))

#dummies that are groups of amenities have to be modified by hand

df$`bathroom essentials` <- ifelse(str_detect(df$amenities, paste(bathroom_essentials, collapse="|")),1,0)

df$`fridge` <- ifelse(str_detect(df$amenities, paste(fridge, collapse="|")),1,0)

df$`oven` <- ifelse(str_detect(df$amenities, "oven"),1,0)

df$`stove` <- ifelse(str_detect(df$amenities, "stove"),1,0)

df$`tv` <- ifelse(str_detect(df$amenities, paste(tv, collapse="|")),1,0)

df$`smart tv` <- ifelse(str_detect(df$amenities, paste(smart_tv, collapse="|")),1,0)

df$`wifi` <- ifelse(str_detect(df$amenities, "wifi"),1,0)

df$`sound system` <- ifelse(str_detect(df$amenities, paste(sound_system, collapse="|")),1,0)

df$`ac` <- ifelse(str_detect(df$amenities, paste(ac, collapse="|")),1,0)

df$`wardrobe` <- ifelse(str_detect(df$amenities, paste(wardrobe, collapse="|")),1,0)

df$`baby kit` <- ifelse(str_detect(df$amenities, paste(baby_kit, collapse="|")),1,0)

df$`toys for children` <- ifelse(str_detect(df$amenities, "children"),1,0)

df$`kitchen amenities` <- ifelse(str_detect(df$amenities, paste(kitchen_amenities, collapse="|")),1,0)

df$`bedroom essentials` <- ifelse(str_detect(df$amenities, paste(bedroom_essentials, collapse="|")),1,0)

df$`reading material` <- ifelse(str_detect(df$amenities, paste(reading_material, collapse="|")),1,0)

df$`washing machine` <- ifelse(str_detect(df$amenities, "washer"),1,0)

df$`dryer` <- ifelse(str_detect(df$amenities, "dryer"),1,0)

df$`exercise equipm` <- ifelse(str_detect(df$amenities, paste(exercise_equip, collapse="|")),1,0)

df$`reserved parking` <- ifelse(str_detect(df$amenities, paste(reserved_parking, collapse="|")),1,0)

df$`unreserved parking` <- ifelse(str_detect(df$amenities, "street parking"),1,0)

df$`games` <- ifelse(str_detect(df$amenities, paste(games, collapse="|")),1,0)

df$`heating` <- ifelse(str_detect(df$amenities, paste(heating, collapse="|")),1,0)

df$`external laundry` <- ifelse(str_detect(df$amenities, "laundromat"),1,0)

df$`essentials` <- ifelse(str_detect(df$amenities, "perfect for the essential"),1,0)

df$`outdoor furniture` <- ifelse(str_detect(df$amenities, paste(outdoor_furniture, collapse="|")),1,0)

df$`special view` <- ifelse(str_detect(df$amenities, paste(special_view, collapse="|")),1,0)

df$`security` <- ifelse(str_detect(df$amenities, paste(security, collapse="|")),1,0)

df$`gym` <- ifelse(str_detect(df$amenities, paste(gym, collapse="|")),1,0)

df$`hot tub` <- ifelse(str_detect(df$amenities, paste(hot_tub, collapse="|")),1,0)

df$`sauna` <- ifelse(str_detect(df$amenities, paste(sauna, collapse="|")),1,0)

df$`pool` <- ifelse(str_detect(df$amenities, paste(pool, collapse="|")),1,0)

df$`table` <- ifelse(str_detect(df$amenities, paste(tables, collapse="|")),1,0)

df$`patio or balcony` <- ifelse(str_detect(df$amenities, "patio or balcony"),1,0)

df$`fire pit` <- ifelse(str_detect(df$amenities, paste(fire_pit, collapse="|")),1,0)

df$`keypad` <- ifelse(str_detect(df$amenities, paste(keypad, collapse="|")),1,0)

df$`windows amenities` <- ifelse(str_detect(df$amenities, paste(windows_amenities, collapse="|")),1,0)

df$`bathtub` <- ifelse(str_detect(df$amenities, "bathtub"),1,0)
df$`bidet` <- ifelse(str_detect(df$amenities, "bidet"),1,0)
df$`bikes` <- ifelse(str_detect(df$amenities, "bikes"),1,0)
df$`breakfast` <- ifelse(str_detect(df$amenities, "breakfast"),1,0)
df$`building staff` <- ifelse(str_detect(df$amenities, "building staff"),1,0)
df$`cleaning available during stay` <- ifelse(str_detect(df$amenities, "cleaning available during stay"),1,0)
df$`cleaning products` <- ifelse(str_detect(df$amenities, "cleaning products"),1,0)
df$`elevator` <- ifelse(str_detect(df$amenities, "elevator"),1,0)
df$`ethernet connection` <- ifelse(str_detect(df$amenities, "ethernet connection"),1,0)
df$`ev charger` <- ifelse(str_detect(df$amenities, "ev charger"),1,0)
df$`iron` <- ifelse(str_detect(df$amenities, "iron"),1,0)
df$`luggage dropoff allowed` <- ifelse(str_detect(df$amenities, "luggage dropoff allowed"),1,0)
df$`pets allowed` <- ifelse(str_detect(df$amenities, "pets allowed"),1,0)
df$`piano` <- ifelse(str_detect(df$amenities, "piano"),1,0)
df$`private entrance` <- ifelse(str_detect(df$amenities, "private entrance"),1,0)
df$`private living room` <- ifelse(str_detect(df$amenities, "private living room"),1,0)
df$`rooftop` <- ifelse(str_detect(df$amenities, "rooftop"),1,0)
df$`smooking allowed` <- ifelse(str_detect(df$amenities, "smoking allowed"),1,0)
df$`suitable for events` <- ifelse(str_detect(df$amenities, "suitable for events"),1,0)
```

### Days since first review
This variable expresses the number of days from the time of the first review to that of the last scraping


```r
#create days since first review
df <- df %>%
  mutate(
    n_days_since = as.numeric(as.Date(calendar_last_scraped,format="%Y-%m-%d") -
                                as.Date(first_review ,format="%Y-%m-%d")))
```



### Host verification
As for amenities, I create dummies related to the type of host verification available


```r
#host verif
df$host_verifications<-gsub("\\[","",df$host_verifications)
df$host_verifications<-gsub("\\]","",df$host_verifications)
df$host_verifications<-gsub("\\'","",df$host_verifications)
df$host_verifications<-gsub(" ","",df$host_verifications)
df$host_verifications<-as.list(strsplit(df$host_verifications, ","))

#define levels and dummies 
levs2 <- levels(factor(unlist(df$host_verifications)))
df<-cbind(df,as.data.frame(do.call(rbind, lapply(lapply(df$host_verifications, factor, levs2), table))))

df <- df %>% 
  rename(host_verif_phone = phone,
         host_verif_email = email,
         host_verif_wemail = work_email
         )
```


### Bathrooms
The variable bathrooms is categorical. I separate the numerical part (expressed by numbers or by "half...") from the description "shared" or "private". For the latter, I create a separate variable "bathrooms_type", which I factorize.


```r
#number of bathrooms
df$bathrooms <- as.numeric(gsub("([0-9]+).*$", "\\1", df$bathrooms_text))
```

```
## Warning: NAs introduced by coercion
```

```r
df$bathrooms[str_detect(df$bathrooms_text, "Half-bath|Shared half-bath")] <- 0.5
#types of bathrooms
df$bathrooms_type[str_detect(df$bathrooms_text, "[Ss]hared")] <- "Shared"
df$bathrooms_type[str_detect(df$bathrooms_text, "[Pp]rivate")] <- "Private"

df <- df %>%
  mutate(f_bathrooms_type = factor(bathrooms_type))
```

### Municipi
Given that the neighboorhoods are too many and too small, I regrouped them in "municipi": this categorization comes from comune.milano.it. I then factorize them.

Note: municipio_2_9 is area between Municipio 2 and Municipio 9, that could not be categorized in any of the two.


```r
#first analyze which neighboorhoods are present
unique(df$neighbourhood_cleansed)
```

```
##  [1] "TIBALDI"                      "NAVIGLI"                     
##  [3] "BUENOS AIRES - VENEZIA"       "XXII MARZO"                  
##  [5] "UMBRIA - MOLISE"              "VIGENTINA"                   
##  [7] "VIALE MONZA"                  "BICOCCA"                     
##  [9] "MAGENTA - S. VITTORE"         "GARIBALDI REPUBBLICA"        
## [11] "CENTRALE"                     "WASHINGTON"                  
## [13] "DE ANGELI - MONTE ROSA"       "VILLAPIZZONE"                
## [15] "PARCO FORLANINI - ORTICA"     "TORTONA"                     
## [17] "DUOMO"                        "BANDE NERE"                  
## [19] "GUASTALLA"                    "LODI - CORVETTO"             
## [21] "GRATOSOGLIO - TICINELLO"      "TICINESE"                    
## [23] "PADOVA"                       "ISOLA"                       
## [25] "PARCO LAMBRO - CIMIANO"       "STADERA"                     
## [27] "GIAMBELLINO"                  "S. CRISTOFORO"               
## [29] "RONCHETTO SUL NAVIGLIO"       "CITTA' STUDI"                
## [31] "BRERA"                        "FORZE ARMATE"                
## [33] "SARPI"                        "LORETO"                      
## [35] "BOVISA"                       "BAGGIO"                      
## [37] "GHISOLFA"                     "BOVISASCA"                   
## [39] "PAGANO"                       "PORTA ROMANA"                
## [41] "GALLARATESE"                  "SCALO ROMANA"                
## [43] "RIPAMONTI"                    "NIGUARDA - CA' GRANDA"       
## [45] "BARONA"                       "LAMBRATE"                    
## [47] "ORTOMERCATO"                  "ADRIANO"                     
## [49] "CORSICA"                      "MACIACHINI - MAGGIOLINA"     
## [51] "EX OM - MORIVIONE"            "AFFORI"                      
## [53] "GRECO"                        "DERGANO"                     
## [55] "FARINI"                       "PORTELLO"                    
## [57] "S. SIRO"                      "QT 8"                        
## [59] "SELINUNTE"                    "LORENTEGGIO"                 
## [61] "MECENATE"                     "QUARTO OGGIARO"              
## [63] "TRIULZO SUPERIORE"            "MAGGIORE - MUSOCCO"          
## [65] "ROGOREDO"                     "PARCO MONLUE' - PONTE LAMBRO"
## [67] "CHIARAVALLE"                  "PARCO SEMPIONE"              
## [69] "PARCO BOSCO IN CITT\u0085"    "TRE TORRI"                   
## [71] "BRUZZANO"                     "FIGINO"                      
## [73] "QUARTO CAGNINO"               "PARCO DELLE ABBAZIE"         
## [75] "PARCO NORD"                   "COMASINA"                    
## [77] "QUINTO ROMANO"                "PARCO DEI NAVIGLI"           
## [79] "GIARDINI PORTA VENEZIA"       "CANTALUPA"                   
## [81] "TRENNO"                       "PARCO AGRICOLO SUD"          
## [83] "QUINTOSOLE"                   "SACCO"                       
## [85] "MUGGIANO"                     "RONCHETTO DELLE RANE"
```

```r
#clean 
df$neighbourhood_cleansed[str_detect(df$neighbourhood_cleansed, "\u0085")] <- "PARCO BOSCO IN CITTA'"

#categorize them into Municipi
Municipio1 <- c("DUOMO", "MAGENTA - S. VITTORE", "GUASTALLA", "TICINESE", "BRERA", "PARCO SEMPIONE")
Municipio2 <- c("VIALE MONZA", "CENTRALE", "PADOVA", "LORETO", "ADRIANO", "GRECO")
Municipio3 <- c("BUENOS AIRES - VENEZIA", "CITTA' STUDI", "PARCO LAMBRO - CIMIANO", "LAMBRATE", "GIARDINI PORTA VENEZIA")
Municipio4 <- c("XXII MARZO", "UMBRIA - MOLISE", "LODI - CORVETTO", "PORTA ROMANA", "ORTOMERCATO", "CORSICA", "MECENATE", "TRIULZO SUPERIORE", "ROGOREDO", "PARCO MONLUE' - PONTE LAMBRO")
Municipio5 <- c("TIBALDI", "VIGENTINA", "GRATOSOGLIO - TICINELLO", "STADERA", "SCALO ROMANA", "RIPAMONTI", "EX OM - MORIVIONE", "CHIARAVALLE", "PARCO DELLE ABBAZIE", "CANTALUPA", "QUINTOSOLE")
Municipio6 <- c("NAVIGLI", "WASHINGTON", "TORTONA", "BANDE NERE", "GIAMBELLINO", "S. CRISTOFORO", "RONCHETTO SUL NAVIGLIO", "BARONA", "LORENTEGGIO", "PARCO DEI NAVIGLI")
Municipio7 <- c("DE ANGELI - MONTE ROSA", "FORZE ARMATE", "BAGGIO", "PAGANO", "S. SIRO", "SELINUNTE", "PARCO BOSCO IN CITTA'", "FIGINO", "QUARTO CAGNINO", "QUINTO ROMANO", "MUGGIANO", "RONCHETTO DELLE RANE")
Municipio8 <- c("VILLAPIZZONE", "PARCO FORLANINI - ORTICA", "SARPI", "GALLARATESE", "GHISOLFA", "PORTELLO", "QT 8", "QUARTO OGGIARO", "MAGGIORE - MUSOCCO", "TRE TORRI", "TRENNO", "SACCO")
Municipio9 <- c("BICOCCA", "GARIBALDI REPUBBLICA", "ISOLA", "BOVISA", "BOVISASCA", "NIGUARDA - CA' GRANDA", "AFFORI", "DERGANO", "FARINI", "BRUZZANO", "PARCO NORD", "COMASINA")
Municipio_2_9 <- c("MACIACHINI - MAGGIOLINA")
Municipio_outside <- c("PARCO AGRICOLO SUD")

#create the municipio variable
df$municipio <- ifelse(df$neighbourhood_cleansed %in% Municipio1, "Municipio1",
                            ifelse(df$neighbourhood_cleansed %in% Municipio2, "Municipio2",
                                   ifelse(df$neighbourhood_cleansed %in% Municipio3, "Municipio3",
                                          ifelse(df$neighbourhood_cleansed %in% Municipio4, "Municipio4",
                                                 ifelse(df$neighbourhood_cleansed %in% Municipio5, "Municipio5",
                                                     ifelse(df$neighbourhood_cleansed %in% Municipio6, "Municipio6",
                                                            ifelse(df$neighbourhood_cleansed %in% Municipio7, "Municipio7",
                                                               ifelse(df$neighbourhood_cleansed %in% Municipio8, "Municipio8",
                                                                      ifelse(df$neighbourhood_cleansed %in% Municipio9, "Municipio9",
                                                                             ifelse(df$neighbourhood_cleansed %in% Municipio_2_9, "Municipio2-9", 
                                                                             ifelse(df$neighbourhood_cleansed %in% Municipio_outside, "Outside",
                                                        ".")))))))))))
#factor
df <- df %>%
  mutate(f_municipio = factor(municipio))
```


#### Drop variables modified
I drop those variables that I modified and I do not need anymore

```r
drops <- c("amenities", "host_verifications", "bathrooms_text", "host_response_rate", "host_acceptance_rate", "municipio", "bathrooms_type")
df <- df[ , !(names(df) %in% drops)]
```

#### rename some variables
For simplicity, I renamed some variables and I keep only those that I need


```r
# add new numeric columns from certain columns
numericals <- c("accommodates","bathrooms","number_of_reviews",
                "reviews_per_month","minimum_nights","beds","bedrooms")
df <- df %>%
  mutate_at(vars(numericals), funs("n"=as.numeric))
```

```
## Warning: Using an external vector in selections was deprecated in tidyselect 1.1.0.
## ℹ Please use `all_of()` or `any_of()` instead.
##   # Was:
##   data %>% select(numericals)
## 
##   # Now:
##   data %>% select(all_of(numericals))
## 
## See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
```

```
## Warning: `funs()` was deprecated in dplyr 0.8.0.
## ℹ Please use a list of either functions or lambdas:
## 
## # Simple named list: list(mean = mean, median = median)
## 
## # Auto named with `tibble::lst()`: tibble::lst(mean, median)
## 
## # Using lambdas list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
```

```r
# rename columns so they start with n_ as opposed to end with _n
nnames <- df %>%
  select(ends_with("_n")) %>%
  names()

nnames_i <- match(nnames, colnames(df))

colnames(df)[nnames_i] <- paste0("n_", numericals)


# create dummy vars with amenities
dummies <- names(df)[seq(47,103)]

df <- df %>%
  mutate_at(vars(dummies), funs("d"= (.)))
```

```
## Warning: Using an external vector in selections was deprecated in tidyselect 1.1.0.
## ℹ Please use `all_of()` or `any_of()` instead.
##   # Was:
##   data %>% select(dummies)
## 
##   # Now:
##   data %>% select(all_of(dummies))
## 
## See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
```

```r
# rename columns
dnames <- df %>%
  select(ends_with("_d")) %>%
  names()
dnames_i <- match(dnames, colnames(df))
colnames(df)[dnames_i] <- paste0("d_", tolower(gsub("[^[:alnum:]_]", "",dummies)))

colnames(df)
```

```
##   [1] "X"                                           
##   [2] "id"                                          
##   [3] "name"                                        
##   [4] "host_id"                                     
##   [5] "host_name"                                   
##   [6] "host_since"                                  
##   [7] "host_location"                               
##   [8] "host_response_time"                          
##   [9] "host_is_superhost"                           
##  [10] "host_listings_count"                         
##  [11] "host_total_listings_count"                   
##  [12] "host_has_profile_pic"                        
##  [13] "host_identity_verified"                      
##  [14] "neighbourhood"                               
##  [15] "neighbourhood_cleansed"                      
##  [16] "property_type"                               
##  [17] "room_type"                                   
##  [18] "accommodates"                                
##  [19] "bathrooms"                                   
##  [20] "bedrooms"                                    
##  [21] "beds"                                        
##  [22] "price"                                       
##  [23] "minimum_nights"                              
##  [24] "maximum_nights"                              
##  [25] "has_availability"                            
##  [26] "calendar_last_scraped"                       
##  [27] "number_of_reviews"                           
##  [28] "number_of_reviews_ltm"                       
##  [29] "number_of_reviews_l30d"                      
##  [30] "first_review"                                
##  [31] "last_review"                                 
##  [32] "review_scores_rating"                        
##  [33] "review_scores_accuracy"                      
##  [34] "review_scores_cleanliness"                   
##  [35] "review_scores_checkin"                       
##  [36] "review_scores_communication"                 
##  [37] "review_scores_location"                      
##  [38] "review_scores_value"                         
##  [39] "instant_bookable"                            
##  [40] "calculated_host_listings_count"              
##  [41] "calculated_host_listings_count_entire_homes" 
##  [42] "calculated_host_listings_count_private_rooms"
##  [43] "calculated_host_listings_count_shared_rooms" 
##  [44] "reviews_per_month"                           
##  [45] "p_host_acceptance_rate"                      
##  [46] "p_host_response_rate"                        
##  [47] "bathroom essentials"                         
##  [48] "fridge"                                      
##  [49] "stove"                                       
##  [50] "oven"                                        
##  [51] "tv"                                          
##  [52] "sound system"                                
##  [53] "smart tv"                                    
##  [54] "ac"                                          
##  [55] "wardrobe"                                    
##  [56] "baby kit"                                    
##  [57] "kitchen amenities"                           
##  [58] "outdoor furniture"                           
##  [59] "bathtub"                                     
##  [60] "bedroom essentials"                          
##  [61] "bidet"                                       
##  [62] "bikes"                                       
##  [63] "games"                                       
##  [64] "reading material"                            
##  [65] "breakfast"                                   
##  [66] "building staff"                              
##  [67] "special view"                                
##  [68] "security"                                    
##  [69] "heating"                                     
##  [70] "table"                                       
##  [71] "toys for children"                           
##  [72] "cleaning available during stay"              
##  [73] "cleaning products"                           
##  [74] "washing machine"                             
##  [75] "dryer"                                       
##  [76] "elevator"                                    
##  [77] "essentials"                                  
##  [78] "ethernet connection"                         
##  [79] "ev charger"                                  
##  [80] "exercise equip"                              
##  [81] "wifi"                                        
##  [82] "reserved parking"                            
##  [83] "unreserved parking"                          
##  [84] "gym"                                         
##  [85] "hot tub"                                     
##  [86] "iron"                                        
##  [87] "keypad"                                      
##  [88] "pool"                                        
##  [89] "external laudry"                             
##  [90] "luggage dropoff allowed"                     
##  [91] "windows amenities"                           
##  [92] "patio or balcony"                            
##  [93] "pets allowed"                                
##  [94] "piano"                                       
##  [95] "private entrance"                            
##  [96] "private living room"                         
##  [97] "sauna"                                       
##  [98] "rooftop"                                     
##  [99] "smoking allowed"                             
## [100] "suitable for events"                         
## [101] "fire pit"                                    
## [102] "exercise equipm"                             
## [103] "external laundry"                            
## [104] "smooking allowed"                            
## [105] "n_days_since"                                
## [106] "host_verif_email"                            
## [107] "host_verif_phone"                            
## [108] "host_verif_wemail"                           
## [109] "f_bathrooms_type"                            
## [110] "f_municipio"                                 
## [111] "n_accommodates"                              
## [112] "n_bathrooms"                                 
## [113] "n_number_of_reviews"                         
## [114] "n_reviews_per_month"                         
## [115] "n_minimum_nights"                            
## [116] "n_beds"                                      
## [117] "n_bedrooms"                                  
## [118] "d_bathroomessentials"                        
## [119] "d_fridge"                                    
## [120] "d_stove"                                     
## [121] "d_oven"                                      
## [122] "d_tv"                                        
## [123] "d_soundsystem"                               
## [124] "d_smarttv"                                   
## [125] "d_ac"                                        
## [126] "d_wardrobe"                                  
## [127] "d_babykit"                                   
## [128] "d_kitchenamenities"                          
## [129] "d_outdoorfurniture"                          
## [130] "d_bathtub"                                   
## [131] "d_bedroomessentials"                         
## [132] "d_bidet"                                     
## [133] "d_bikes"                                     
## [134] "d_games"                                     
## [135] "d_readingmaterial"                           
## [136] "d_breakfast"                                 
## [137] "d_buildingstaff"                             
## [138] "d_specialview"                               
## [139] "d_security"                                  
## [140] "d_heating"                                   
## [141] "d_table"                                     
## [142] "d_toysforchildren"                           
## [143] "d_cleaningavailableduringstay"               
## [144] "d_cleaningproducts"                          
## [145] "d_washingmachine"                            
## [146] "d_dryer"                                     
## [147] "d_elevator"                                  
## [148] "d_essentials"                                
## [149] "d_ethernetconnection"                        
## [150] "d_evcharger"                                 
## [151] "d_exerciseequip"                             
## [152] "d_wifi"                                      
## [153] "d_reservedparking"                           
## [154] "d_unreservedparking"                         
## [155] "d_gym"                                       
## [156] "d_hottub"                                    
## [157] "d_iron"                                      
## [158] "d_keypad"                                    
## [159] "d_pool"                                      
## [160] "d_externallaudry"                            
## [161] "d_luggagedropoffallowed"                     
## [162] "d_windowsamenities"                          
## [163] "d_patioorbalcony"                            
## [164] "d_petsallowed"                               
## [165] "d_piano"                                     
## [166] "d_privateentrance"                           
## [167] "d_privatelivingroom"                         
## [168] "d_sauna"                                     
## [169] "d_rooftop"                                   
## [170] "d_smokingallowed"                            
## [171] "d_suitableforevents"                         
## [172] "d_firepit"                                   
## [173] "d_exerciseequipm"                            
## [174] "d_externallaundry"
```

```r
df2 <- df %>%
  select(name, id, price,
         neighbourhood_cleansed, room_type, property_type, review_scores_rating, matches("^n_.*|^f_.*|^p_.*|^d_.*"))
```


# Save and write 
I save a new csv file with data cleaned


```r
#write csv
write.csv(df2,file="airbnb_milano_cleaned.csv")
```


library(readr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(treemapify)


# data source: https://data.cityofchicago.org/Public-Safety/Crimes-2022/9hwr-2zxp

data <- read_csv("CrimesChicago_2022.csv")
data$Date <- parse_date_time(data$Date, "%m/%d/%y %I:%M:%S %p")
data$month <- format(as.Date(data$Date), "%m")
data$Month <- month.abb[as.integer(data$month)]
colnames(data)
table(data$`Location Description`)

data$`Location Description` <- dplyr::case_when(data$`Location Description` %in% c("AIRCRAFT", "BOAT / WATERCRAFT", "COIN OPERATED MACHINE", "OTHER (SPECIFY)", "VESTIBULE", "FARM") ~ "OTHER",
                                                data$`Location Description` %in% c("AIRPORT BUILDING NON-TERMINAL - NON-SECURE AREA", "AIRPORT BUILDING NON-TERMINAL - SECURE AREA", "AIRPORT EXTERIOR - NON-SECURE AREA", 
                                                                                   "AIRPORT EXTERIOR - SECURE AREA", "AIRPORT PARKING LOT", "AIRPORT TERMINAL LOWER LEVEL - NON-SECURE AREA", 
                                                                                   "AIRPORT TERMINAL LOWER LEVEL - SECURE AREA", "AIRPORT TERMINAL MEZZANINE - NON-SECURE AREA", "AIRPORT TERMINAL UPPER LEVEL - NON-SECURE AREA", 
                                                                                   "AIRPORT TERMINAL UPPER LEVEL - SECURE AREA", "AIRPORT TRANSPORTATION SYSTEM (ATS)", "AIRPORT VENDING ESTABLISHMENT", 
                                                                                   "AIRPORT/AIRCRAFT", "FEDERAL BUILDING", "FIRE STATION", "POLICE FACILITY / VEHICLE PARKING LOT", "GOVERNMENT BUILDING / PROPERTY", 
                                                                                   "POLICE FACILITY", "JAIL / LOCK-UP FACILITY", "LIBRARY") ~ 'GOVERNMENT FACILITY',
                                                data$`Location Description` %in% c("ALLEY", "BRIDGE", "CEMETARY", "PARK PROPERTY", "FOREST PRESERVE", "LAKEFRONT / WATERFRONT / RIVERBANK", 
                                                                                   "RIVER BANK", "SIDEWALK", "STAIRWELL", "STREET", "HIGHWAY / EXPRESSWAY") ~ 'OPEN PUBLIC SPACE',
                                                data$`Location Description` %in% c("CHA PARKING LOT", "CHA PARKING LOT / GROUNDS", "DRIVEWAY", "DRIVEWAY - RESIDENTIAL", "PARKING LOT", 
                                                                                   "PARKING LOT / GARAGE (NON RESIDENTIAL)", "GARAGE", "PORCH") ~ 'OPEN PRIVATE SPACE',
                                                data$`Location Description` %in% c("ANIMAL HOSPITAL", "HOSPITAL BUILDING / GROUNDS", "MEDICAL / DENTAL OFFICE", "NURSING / RETIREMENT HOME") ~ "MEDICAL FACILITY", 
                                                data$`Location Description` %in% c("ABANDONED BUILDING", "CONSTRUCTION SITE", "VACANT LOT", "VACANT LOT / LAND") ~ "VACANT LOT/SPACE", 
                                                data$`Location Description` %in% c("APARTMENT", "CHA APARTMENT", "CHA HALLWAY / STAIRWELL / ELEVATOR", "CHA LOBBY", "RESIDENCE", "HOUSE", "RESIDENCE - GARAGE", 
                                                                                   "RESIDENCE - PORCH / HALLWAY", "RESIDENCE - YARD (FRONT / BACK)", "HALLWAY" ) ~ "RESIDENCE", 
                                                data$`Location Description` %in% c("ATM (AUTOMATIC TELLER MACHINE)", "BANK", "CREDIT UNION", "CURRENCY EXCHANGE", "SAVINGS AND LOAN") ~ "BANK",
                                                data$`Location Description` %in% c("AUTO", "TAXICAB", "TRUCK", "VEHICLE - COMMERCIAL", "VEHICLE - COMMERCIAL: ENTERTAINMENT / PARTY BUS", 
                                                                                   "VEHICLE - COMMERCIAL: TROLLEY BUS", "VEHICLE - DELIVERY TRUCK", "VEHICLE - OTHER RIDE SHARE SERVICE (LYFT, UBER, ETC.)", 
                                                                                   "VEHICLE NON-COMMERCIAL") ~ "VEHICLE",
                                                data$`Location Description` %in% c("COLLEGE / UNIVERSITY - GROUNDS", "COLLEGE / UNIVERSITY - RESIDENCE HALL", "DAY CARE CENTER", "PUBLIC GRAMMAR SCHOOL", 
                                                                                   "SCHOOL - PRIVATE BUILDING", "SCHOOL - PRIVATE GROUNDS", "SCHOOL - PUBLIC BUILDING", "SCHOOL - PUBLIC GROUNDS") ~ "SCHOOL",
                                                data$`Location Description` %in% c("CHURCH / SYNAGOGUE / PLACE OF WORSHIP") ~ "CHURCH",
                                                data$`Location Description` %in% c("FACTORY / MANUFACTURING BUILDING", "WAREHOUSE", "YARD") ~ "INDUSTRIAL SPACE",
                                                data$`Location Description` %in% c("SPORTS ARENA / STADIUM", "MOVIE HOUSE / THEATER") ~ "VENUE", 
                                                data$`Location Description` %in% c('CTA "L" TRAIN', "CTA BUS", "CTA BUS STOP", "CTA PARKING LOT / GARAGE / OTHER PROPERTY", "CTA PLATFORM", "CTA PROPERTY ", "CTA STATION", 
                                                                                   "CTA TRACKS - RIGHT OF WAY", "CTA TRAIN", "OTHER RAILROAD PROPERTY / TRAIN DEPOT", "RAILROAD PROPERTY", "OTHER COMMERCIAL TRANSPORTATION") ~ "PUBLIC TRANSIT", 
                                                data$`Location Description` %in% c("APPLIANCE STORE", "ATHLETIC CLUB", "AUTO / BOAT / RV DEALERSHIP" , "BANQUET HALL", "BAR OR TAVERN", "BARBER SHOP/BEAUTY SALON", 
                                                                                   "BARBERSHOP", "BOWLING ALLEY", "CAR WASH", "CLEANING STORE", "COMMERCIAL / BUSINESS OFFICE", "CONVENIENCE STORE", 
                                                                                   "DEPARTMENT STORE", "DRUG STORE", "GAS STATION", "GROCERY FOOD STORE", "PAWN SHOP", "POOL ROOM", "LIQUOR STORE", "HOTEL", 
                                                                                   "HOTEL / MOTEL", "RESTAURANT", "RETAIL STORE", "TAVERN", "TAVERN / LIQUOR STORE", "SMALL RETAIL STORE") ~ "COMMERCIAL PROPERTY")

table(data$`Location Description`)


data_state = data %>% group_by(State, Segment)  %>%
  summarise(total_sales = sum(Sales),
            total_profits = sum(Profit),
            avg_discount = mean(Discount),
            .groups = 'drop')

nrow(data_state)

hist(x, breaks = bins, col = 'darkgray', border = 'white',
     xlab = 'Waiting time to next eruption (in mins)',
     main = 'Histogram of waiting times')

colnames(data)

# Graph 1
ggplot(data, aes(x = reorder(Month, as.integer(month)))) +
  geom_bar()

# a esta grafica le debemos poder apicar un filtro en data para ver solamente cierto tipo de crimenes
# Graph 2
data$month

count_type <- count(data, `Primary Type`)

ggplot(count_type, aes(area = n, label = `Primary Type` , fill = n)) +
  geom_treemap() +
  geom_treemap_text(fontface = "bold", colour = "white", place = "centre", 
                    reflow = TRUE, min.size = 3)




library(shiny)
profit <- data$Profit
bins <- seq(min(profit), max(profit), length.out = input$bins + 1)

hist(data_state$total_sales)

?strptime()
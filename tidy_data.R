
library(tidyverse)
library(magrittr)

## load data
strawb <- read.csv("Strawberries.csv",fileEncoding = "latin1", header = TRUE)
pest <- read.csv("Pesticides.csv", fileEncoding = "latin1", header = TRUE)
##################################################
## Drop the no-info columns
#################################################
## capture column names
cnames <- colnames(strawb)


x <- 1:dim(strawb)[2]



## set variable to collect values

T <- NULL

## collect number of unique rows in each column

for(i in x){T <- c(T, dim(unique(strawb[i]))[1])}


## use T to select columns to drop
drop_cols <- cnames[which(T == 1)]


## drop strawb
strawb %<>% select(!all_of(drop_cols))



drop_no_info_cols <- function(df){
  cnames = colnames(strawb)
  T = NULL
  for(i in 1:ncol(df)){T <- c(T, nrow(unique(df[i])))}
  drop_cols <- cnames[which(T == 1)]
  return(select(df, !all_of(drop_cols)))
}

strawb <- drop_no_info_cols(strawb)

##################################################
## Separate of column
##################################################

## Separate Data.Item into 4 columns
strawb %<>% separate(col=Data.Item,
                     into = c("Strawberries", "items", "description", "units"), 
                     sep = ",",
                     fill = "right")

strawb %<>% separate(col=items,
               into = c("items", "measurement" ), 
               sep = "-", 
               fill = "right")

## Separate Domain into 2 columns
strawb %<>%  separate(col=Domain,
                      into = c("dname", "type" ), 
                      sep = ",", 
                      fill = "right")

strawb$description[!is.na(strawb$measurement)] <-  paste(strawb$measurement[!is.na(strawb$measurement)], ",", strawb$description[!is.na(strawb$measurement)])


strawb$measurement <- NULL #remove measurement first
strawb$description <- str_replace(strawb$description, ", NA", "")

strawb$units <- NULL#not use this column
strawb$State.ANSI <- NULL


#simply remove TOTAL level data
strawb %<>% 
  filter(Strawberries == 'STRAWBERRIES' & Period == 'YEAR')

#put value = 0 in data with (D)
strawb$Value <- str_replace(strawb$Value, "\\s\\(D\\)", "")
strawb$Value <- str_replace(strawb$Value, '\\s\\(NA\\)', "")
strawb$Value <- str_replace_all(strawb$Value, ',', "")
strawb$Value <- str_replace_all(strawb$Value, '\\s\\(Z\\)', "")
strawb$Value[strawb$Value == ''] <- '0'

#convert it to numeric
strawb$Value <- as.numeric(strawb$Value)


###################################
## make a copy of Domain.Category

strawb %<>% 
  mutate(Chemicals = Domain.Category) %>% 
  relocate(Chemicals, .after = Domain.Category) 


## vector of logicals for each row with "CHEM" at 
## the start of strawb$Chemicals

bb <- strawb$Chemicals %>% str_detect("CHEM") 

sum(bb)

## index 
ind_C <- (!bb)*(1:dim(strawb)[1]) # not start with 'CHEM',multiply the # of rows in strawb data
#get the index that is not start with CHEM

## 
r1 <- ind_C[ind_C > 0] #get the number of rows which chemicals column not start with chemical

## set entries in Chemicals column to " " if they don't start with CHEM

strawb$Chemicals[r1] <- " "

##################################
###get a list of chemicals
#########################################


strawb %<>% separate(col = Chemicals,
                     into = c("title", "details"),
                     sep = ":",
                     fill = "right")

strawb %<>% mutate(details = str_extract(str_trim(details) ,"[^(].*[^)]") )

strawb %<>% mutate(type = str_trim(type))

#################################

##get rid of "=" and index
strawb$details <- str_replace_all(strawb$details, "=\\s\\d+$", '') 
#NA is from the previous NA in starwb$details
#details now will be the chemical type, if they are chemical

#drop domain category
strawb$Domain.Category <- NULL

#drop title (duplicate information in type)
strawb$title <- NULL

#drop items(duplicate information in dnames)
strawb$items <- NULL

#drop cv(not use)
strawb$CV.... <- NULL

#drop straberries
strawb$Strawberries <- NULL

#drop Period
#strawb$Year <- NULL

################
#deal with the space in chemical details
strawb$details <- str_replace_all(strawb$details, " ", "_")
#remove the last _
strawb$details <- substr(strawb$details,1 ,nchar(strawb$details)-1)


#######
strawb$type[strawb$dname == 'FERTILIZER'] <- 'FERTILIZER'
strawb$dname[strawb$type == 'FERTILIZER'] <- 'CHEMICAL'

write.csv(strawb, 'strawb_organic.csv')


pest <- pest %>% mutate_all(.funs = toupper) %>% drop_no_info_cols()
pest <- pest[!apply(pest == "", 1, all),] %<>% rename(chem_name = ï..Pesticide)

#use _ to substitue space
pest$chem_name <- str_replace_all(pest$chem_name, " ", "_")

#There's no abbreviation in strawb data set.
pest$chem_name <- str_replace_all(pest$chem_name, "\\_\\(\\w+\\)", "")


#check if any chem_name in pest are in strawb$details
pest$chem_name[!pest$chem_name %in% strawb$details]

#TETRAHYDROPHTHALIMIDE not in strawb$detaisl
#SPINOSAD_D and SPINOSAD_A are call SPINOSAD in strawb, since there are no entry in pest, just combine it. 
# THIABENDAZOLE,TEBUCONAZOLE,CARBENDAZIM,MALAOXON(similar one, but not the same) not in
# PROPICONAZOLE_II, PROPICONAZOLE_I drop them, PROPICONAZOLE match in strawb
# use ENDOSULFAN in pest, combine I,II, drop ENDOSULFAN_S...
# OXAMYL_OXIME not QUINOXYFEN not in DICHLORVOS not DIMETHOATE not in 

#####################################
#add chem_name not in pest but in strawb
#change PROPICONAZOLE_I and PROPICONAZOLE_II to PROPICONAZOLE
pest$chem_name[pest$chem_name == 'PROPICONAZOLE_I'] <- 'PROPICONAZOLE'
pest$chem_name[pest$chem_name == 'ENDOSULFAN_I'] <- 'ENDOSULFAN'
pest$chem_name[pest$chem_name == 'SPINOSAD_D'] <- 'SPINOSAD'
pest <- subset(pest, !chem_name%in% pest$chem_name[!pest$chem_name %in% strawb$details])

#check it again
pest$chem_name[!pest$chem_name %in% strawb$details]

#add chem_name not in pest but in strawb
unique(strawb$details[!strawb$details %in% pest$chem_name])

df <- data.frame(unique(strawb$details[!strawb$details %in% pest$chem_name])[-1],rep("UNKNOWN",124),rep("UNKNOWN",124),rep("UNKNOWN",124),rep("UNKNOWN",124),rep("UNKNOWN",124))
colnames(df) <- colnames(pest)



strawb %>% 
  filter(details %in% unique(strawb$details[!strawb$details %in% pest$chem_name]) & !is.na(details)) %>%
  nrow()

#assign unknown
for(i in 2:6){
  pest[pest[,i]== '',i] <- "UNKNOWN"
}

pest <-  rbind(pest, df)


#joined data
strawb$chem_name <- strawb$details
strawb$type[strawb$dname == 'Fertilizer'] <- 'FERTILIZER'

dt <- merge(strawb, pest, x.join = "chem_name")

head(dt)

#0 -> not toxic 1 -> toxic

dt$toxicity_human_bee <- ifelse(dt$Bee.Toxins != 'UNKNOWN', 1, 
                                ifelse(dt$Carcinogen == 'UNKNOWN'& dt$Hormone.Disruptor == 'UNKNOWN'&dt$Neurotoxins == 'UNKNOWN'&dt$Developmental.or.Reproductive.Toxins == 'UNKNOWN', 0, 1))

dt$dname <- NULL
dt$Period <- NULL
dt$details <- NULL
dt <- rename(dt, c(chemical = chem_name, program = Program, state = State, measurements = description, value = Value, carcinogen = Carcinogen, hormone_disruptor= Hormone.Disruptor, neurotoxins = Neurotoxins, developmental_or_reproductive_toxins =  Developmental.or.Reproductive.Toxins,bee_toxins = Bee.Toxins ))

write.csv(dt, 'strawb.csv')





#importing the dataset
dd=read.csv("C:\\Users\\kivanova4\\Documents\\Business Analytics\\Business Analytics\\2 semestur\\Data_prep\\day2\\cars.csv", stringsAsFactors = FALSE)

head(dd)
dim(dd) # 398  10
class(dd) #data.frame

#check for missing values
sapply(dd, function(x) sum(is.na(x)))

# sapply(dd, function(x) sum(is.na(x)))
# X      car.name           mpg     cylinders  displacement    horsepower        weigth accelelration 
# 0             0             0             0             0             0             0             0 
# modelYear        origin 
# 0             0


#change the order of the columns (mpg to be on first place)
colnames(dd)
dd1 <- dd[, c(3, 1, 2, 4, 5, 6, 7, 8, 9, 10)]
head(dd1)

#find the class of each variable
sapply(dd1,class)

# > apply(dd1,2,class)
# mpg             X      car.name     cylinders  displacement    horsepower        weigth accelelration 
# "character"   "character"   "character"   "character"   "character"   "character"   "character"   "character" 
# modelYear        origin 
# "character"   "character" 

#chnanging the class of each column to an appropriate one
dd1$mpg <- as.integer(dd1$mpg)
dd1$X <- as.integer(dd1$X)
dd1$car.name <- as.character(dd1$car.name)
dd1$cylinders <- as.integer(dd1$cylinders)
dd1$displacement <- as.integer(dd1$displacement)
dd1$horsepower <- as.integer(dd1$horsepower)
dd1$weigth <- as.integer(dd1$weigth)
dd1$accelelration <- as.numeric(dd1$accelelration)
dd1$modelYear <- as.integer(dd1$modelYear)
dd1$origin <- as.integer(dd1$origin)

# > sapply(dd1,class)
# mpg             X      car.name     cylinders  displacement    horsepower        weigth accelelration 
# "integer"     "integer"      "factor"     "integer"     "integer"     "integer"     "integer"     "numeric" 
# modelYear        origin 
# "integer"     "integer" 

levels(dd1$car.name)
levels(dd1$horsepower) #NULL???

#Create a new factor variable BRAND
class(dd1$car.name)

brands=list() #1st thing we create an empty object

brands=strsplit(dd1$car.name,split="[ ]")
str(brands)
dd1$brand=sapply(brands, "[",1)
class(dd1$brand)   #character
dd1$brand=as.factor(dd1$brand)
levels(dd1$brand)

# [1] "amc"           "audi"          "bmw"           "buick"         "cadillac"      "capri"         "chevroelt"    
# [8] "chevrolet"     "chevy"         "chrysler"      "datsun"        "dodge"         "fiat"          "ford"         
# [15] "hi"            "honda"         "maxda"         "mazda"         "mercedes"      "mercedes-benz" "mercury"      
# [22] "nissan"        "oldsmobile"    "opel"          "peugeot"       "plymouth"      "pontiac"       "renault"      
# [29] "saab"          "subaru"        "toyota"        "toyouta"       "triumph"       "vokswagen"     "volkswagen"   
# [36] "volvo"         "vw

#We note there are typos. As next we apply table to see the frequency
# You might wish to correct typos:
dd$brand=as.character(dd$brand)
dd$brand[dd$brand=="vw"|dd$brand=="vokswagen"]="volkswagen"
dd$brand[dd$brand=="chevroelt"|dd$brand=="chevy"]="chevrolet"
dd$brand[dd$brand=="maxda"]="mazda"
dd$brand[dd$brand=="toyouta"]="toyota"
dd$brand[dd$brand=="mercedes-benz"]="mercedes"
dd$brand=as.factor(dd$brand)
table(dd1$brand)
# 
# amc          audi           bmw         buick      cadillac         capri     chevroelt     chevrolet 
# 28             7             2            17             2             1             1            43 
# chevy      chrysler        datsun         dodge          fiat          ford            hi         honda 
# 3             6            23            28             8            51             1            13 
# maxda         mazda      mercedes mercedes-benz       mercury        nissan    oldsmobile          opel 
# 2            10             1             2            11             1            10             4 
# peugeot      plymouth       pontiac       renault          saab        subaru        toyota       toyouta 
# 8            31            16             5             4             4            25             1 
# triumph     vokswagen    volkswagen         volvo            vw 
# 1             1            15             6             6

freq=data.frame(table(dd$brand))
colnames(freq)[1]="brand"


freq=freq[order(freq$Freq, decreasing = T),] # wich including order we order the  freg in decreasing order
freq[1:5,]

# brand Freq
# 14      ford   51
# 8  chevrolet   43
# 26  plymouth   31
# 1        amc   28
# 12     dodge   28

# Adding a column "freq" to our main dataframe  'dd1'
dd1=merge(dd1,freq,by="brand")
min(freq$Freq[1:5]) # [1] 28

# Subsample by dd$Freq>=28 (a way to filter the data we would like to work with)
dds=dd1[dd1$Freq>=28,]

summary(dds)



# Let's illustrate the application of brand in a linear model
eq=lm(mpg~brand,data=dd1)


#box graphs?

#creating a linear regression model with MPG as a depengind variable
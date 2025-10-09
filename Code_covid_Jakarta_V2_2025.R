library("readxl")  
library("dplyr")
library("stringr")
library("ggplot2")
library("viridis") # Import  nice color palette

cat("\014") 
rm(list = ls())
graphics.off()
df <- read_excel("C:\\Users\\L03565094\\Dropbox\\Francisco\\Papers_SUTD\\Sharing_Economy\\Simulation\\Platform\\Covid\\Code2021\\Long2\\Covid_Jakarta_and_population.xlsx", sheet = "Cases_Mar2020_to_Jun2021")

df <- subset(df, df$day!=0)  
#--------------------------------------------------------------
#     Fix in here the mistmatch between positive cases
#--------------------------------------------------------------

df <- subset(df, df$ID_KEL!="PROSES UPDATE DATA")
df$day <- str_pad(df$day, 2, pad = "0")
df$unique_id <- paste(df$ID_KEL, df$day, sep="-")
df <- df[order(df$unique_id),]
df$date <- as.POSIXct(strptime(df$date, "%Y-%m-%d"))
df <- subset(df, df$ID_KEL!="LUAR DKI JAKARTA")
df <- subset(df, df$ID_KEL!="BELUM DIKETAHUI")

####### Calculating Lead, difference between current row case and lead case, assigning that difference to the df
df<- 
  df %>%
  group_by(ID_KEL) %>%
  mutate(positive_lead = dplyr::lead(positive, n = 1, default = NA))
lead_diff_positive <- df$positive_lead - df$positive
df$lead_diff_positive <-lead_diff_positive 

####### Mistakes fixing: If the following date's case number is lower than the preceding date's case number, 
####### replace the preceding date's case number with the following date's case number
###### The while loop will repeat this process until no following date's case number is lower than the preceding number

condition_for_change<- df$lead_diff_positive <0 & !is.na(df$lead_diff_positive)

success<-FALSE
while(!success)
{
  df$positive[condition_for_change]<-df$positive_lead[condition_for_change]
  df<- 
    df %>%
    group_by(ID_KEL) %>%
    mutate(positive_lead = dplyr::lead(positive, n = 1, default = NA))
  lead_diff_positive <- df$positive_lead-df$positive
  df$lead_diff_positive <-lead_diff_positive 
  condition_for_change<- df$lead_diff_positive <0 & !is.na(df$lead_diff_positive)
  break_condition<-all(df$lead_diff_positive>=0,na.rm=TRUE)
  if(break_condition) break
}

####### Re-calculating positive_lag and diff_positive:
df<- 
  df %>%
  group_by(ID_KEL) %>%
  mutate(positive_lag = dplyr::lag(positive, n = 1, default = NA))
df$diff_positive <- df$positive - df$positive_lag
####### Checking if all mistakes are fixed
df_negative <- subset(df, df$diff_positive<0)



######## Let's remove the columns we don't need
df$day <- NULL
df$cum_per_day <- NULL
df$unique_id <- NULL
df$positive_lead <- NULL
df$lead_diff_positive <- NULL
df$positive_lag <- NULL
df$diff_positive <- NULL



#######################################################
#######################################################
##################      PART I      ###################
#######################################################
#######################################################

#--------------------------------------------------------------
### Generating plots
# In this ection we are going to use cumulative COVID-19 cases
# of every Saturday between 28 Apr 00 to 24 Apr 21
#--------------------------------------------------------------
df <- subset(df, (df$date=="2020-03-28" | df$date=="2020-04-04" | df$date=="2020-04-11" |
                    df$date=="2020-04-18" | df$date=="2020-04-25" | df$date=="2020-05-02" |
                    df$date=="2020-05-09" | df$date=="2020-05-16" | df$date=="2020-05-23" | 
                    df$date=="2020-05-30" | df$date=="2020-06-06" | df$date=="2020-06-13" |
                    df$date=="2020-06-20" | df$date=="2020-06-27" | df$date=="2020-07-04" |
                    df$date=="2020-07-11" | df$date=="2020-07-18" | df$date=="2020-07-25" |
                    df$date=="2020-08-01" | df$date=="2020-08-08" | df$date=="2020-08-15" |
                    df$date=="2020-08-22" | df$date=="2020-08-29" | df$date=="2020-09-05" |
                    df$date=="2020-09-12" | df$date=="2020-09-19" | df$date=="2020-09-26" |
                    df$date=="2020-10-03" | df$date=="2020-10-10" | df$date=="2020-10-17" |
                    df$date=="2020-10-24" | df$date=="2020-10-31" | df$date=="2020-11-07" |
                    df$date=="2020-11-14" | df$date=="2020-11-21" | df$date=="2020-11-28" |
                    df$date=="2020-12-05" | df$date=="2020-12-12" | df$date=="2020-12-19" |
                    df$date=="2020-12-26" | df$date=="2021-01-02" | df$date=="2021-01-09" |
                    df$date=="2021-01-16" | df$date=="2021-01-23" | df$date=="2021-01-30" |
                    df$date=="2021-02-06" | df$date=="2021-02-13" | df$date=="2021-02-20" |
                    df$date=="2021-02-27" | df$date=="2021-03-06" | df$date=="2021-03-13" |
                    df$date=="2021-03-20" | df$date=="2021-03-27" | df$date=="2021-04-03" |
                    df$date=="2021-04-10" | df$date=="2021-04-17" | df$date=="2021-04-24") ) ## Add weeks till April 2021


### We need to calculate the difference in total number of cases between any 
# two Ssturdays. We are interested in how many new cases per week are reported
df<- 
  df %>%
  group_by(ID_KEL) %>%
  mutate(positive_lag = dplyr::lag(positive, n = 1, default = NA))

df$diff_positive <- df$positive - df$positive_lag



### Let's not append the population of each kelurahan. 
## The correct way to analyze the new cases is by  weighting according to the total pulation
pop <- read_excel("C:\\Users\\L03565094\\Dropbox\\Francisco\\Papers_SUTD\\Sharing_Economy\\Simulation\\Platform\\Covid\\Code2021\\Long2\\Covid_Jakarta_and_population.xlsx", sheet = "population")
pop <- pop[,c("ID_KEL","total_pop","area_km2_in_land","pop_density_ppl_per_sq_km","FID")]
df <- merge(x=df, y=pop, by="ID_KEL", all.x=T)

#--------------------------------------------------------------
#The above variable is our main variable of interest. It measures the total number of
#new cases per week per 1,000 people. 
#For example, kelurahan `PULAU PANGGANG` reported 0.2880184 new cases per 1,000 people 
# over the week 11-18 July 2020. Alternatively, kelurahan `GALUR` reported 1.72 new cases
# per 1,000 people over the week 8-15 Aug 2020.
df$positive_new_per_week_per_1000 <- df$diff_positive / (df$total_pop/1000)
#--------------------------------------------------------------




#--------------------------------------------------------------
# Our main hypotheses is: Kelurahans with high population density correlate to
#                         high infection rate
#                         (e.g., positive new cases per week per 1000 people)?
#  To do so, we need to classify each kelurahan in one of the following groups
#
#                         I. High infection - High pop. density 
#                        II. Low infection - Low pop. density 
#                       III. Low infection - High pop. density 
#                        IV. High infection - Low pop. density 
#--------------------------------------------------------------


######### Let's aggregate the information per month, and instead of computing the
# positive new cases per week per 1000 people we calculate 
# average positive new cases per month per 1000 people
df_avg_months <- df[,c("nama_kelurahan","positive_new_per_week_per_1000","date")]
df_avg_months$month <- str_sub(df_avg_months$date,1,7)
df_avg_months <- subset(df_avg_months, (df_avg_months$month!="2020-03" )) # Let's remove March 2020 as it only has data of one week that we cannot use
df_avg_months$date <- NULL
df_avg_months <- aggregate(list(positive_new_per_week_per_1000=df_avg_months$positive_new_per_week_per_1000),
                           by=list(nama_kelurahan=df_avg_months$nama_kelurahan, month=df_avg_months$month),
                           mean, na.action = na.omit)  # This function agregates the weekly date into avg. per month


##--------------------------
# The next lines of code are just to assign colors to kelurahans where we have survey data
df_avg_months$color1 <- ifelse(df_avg_months$nama_kelurahan=="CENGKARENG BARAT", "02 - Cengkareng", "")
df_avg_months$color2 <- ifelse( (df_avg_months$nama_kelurahan=="RAWA BADAK SELATAN" | df_avg_months$nama_kelurahan=="TUGU SELATAN" |
                                   df_avg_months$nama_kelurahan=="KELAPA GADING BARAT" ), "03 - Tanah Merah", "")
df_avg_months$color3 <- ifelse( (df_avg_months$nama_kelurahan=="PETAMBURAN" | df_avg_months$nama_kelurahan=="BENDUNGAN HILIR" ), "04 - Benhil","")
df_avg_months$color4 <- ifelse( (df_avg_months$nama_kelurahan=="PADEMANGAN BARAT" ), "05 - PADEMANGAN BARAT","")

df_avg_months$color <- paste(df_avg_months$color1, df_avg_months$color2,
                             df_avg_months$color3,df_avg_months$color4, sep="")
df_avg_months$color_all <- ifelse(df_avg_months$color=="", "01 - Others",df_avg_months$color)
df_avg_months$color1 <- NULL
df_avg_months$color2 <- NULL
df_avg_months$color3 <- NULL
df_avg_months$color4 <- NULL
##----------------------------



##----------------------------
# The lines below are very important. They calculate the average weekly new cases PER MONTH
df$pop_density<- df$pop_density_ppl_per_sq_km
pop_density <- df[,c("nama_kelurahan","pop_density")]
pop_density <- unique(pop_density)
df_avg_months <- merge(x=df_avg_months, y=pop_density, by="nama_kelurahan", all.x=T)
df_avg_months$avg_per_month <- ave(df_avg_months$positive_new_per_week_per_1000, df_avg_months$month, FUN=mean)
##----------------------------

##----------------------------
# In this lines we only re-label the date. We replace month number by text
df_avg_months$month <- ifelse(df_avg_months$month=="2020-04", "Apr-20", df_avg_months$month)
df_avg_months$month <- ifelse(df_avg_months$month=="2020-05", "May-20", df_avg_months$month)
df_avg_months$month <- ifelse(df_avg_months$month=="2020-06", "Jun-20", df_avg_months$month)
df_avg_months$month <- ifelse(df_avg_months$month=="2020-07", "Jul-20", df_avg_months$month)
df_avg_months$month <- ifelse(df_avg_months$month=="2020-08", "Aug-20", df_avg_months$month)
df_avg_months$month <- ifelse(df_avg_months$month=="2020-09", "Sep-20", df_avg_months$month)
df_avg_months$month <- ifelse(df_avg_months$month=="2020-10", "Oct-20", df_avg_months$month)
df_avg_months$month <- ifelse(df_avg_months$month=="2020-11", "Nov-20", df_avg_months$month)
df_avg_months$month <- ifelse(df_avg_months$month=="2020-12", "Dec-20", df_avg_months$month)
df_avg_months$month <- ifelse(df_avg_months$month=="2021-01", "Jan-21", df_avg_months$month)
df_avg_months$month <- ifelse(df_avg_months$month=="2021-02", "Feb-21", df_avg_months$month)
df_avg_months$month <- ifelse(df_avg_months$month=="2021-03", "Mar-21", df_avg_months$month)
df_avg_months$month <- ifelse(df_avg_months$month=="2021-04", "Apr-21", df_avg_months$month)
df_avg_months$month <- factor(df_avg_months$month, 
                              levels = c("Apr-20", "May-20", "Jun-20", "Jul-20",
                                         "Aug-20", "Sep-20", "Oct-20", "Nov-20",
                                         "Dec-20", "Jan-21", "Feb-21", "Mar-21",
                                         "Apr-21")) # This line is important because here, 
                                                    # we specify the order of the months. Otherwise R sort alphabetically
##----------------------------



##----------------------------
## Ok the next step is plot the data of Apr20, Jul20, Oct20, Jan21, Apr21
df_avg_months_nicer <- df_avg_months
df_avg_months_nicer <- subset(df_avg_months_nicer, 
                              (df_avg_months_nicer$month=="Apr-20"|
                               df_avg_months_nicer$month=="Jul-20"|
                               df_avg_months_nicer$month=="Oct-20"|
                               df_avg_months_nicer$month=="Jan-21"|
                               df_avg_months_nicer$month=="Apr-21"))

df_avg_months_nicer$color_all <- ifelse(df_avg_months_nicer$color_all=="05 - PADEMANGAN BARAT","01 - Others",df_avg_months_nicer$color_all)
df_avg_months_nicer$color_all <- ifelse(df_avg_months_nicer$color_all=="02 - Cengkareng","06 - Surveyed",df_avg_months_nicer$color_all)
df_avg_months_nicer$color_all <- ifelse(df_avg_months_nicer$color_all=="04 - Benhil","06 - Surveyed",df_avg_months_nicer$color_all)



df_avg_months_nicer$size <- ifelse(df_avg_months_nicer$color_all=="01 - Others", 1, 3)

setwd("C:\\Users\\L03565094\\Dropbox\\Francisco\\Papers_SUTD\\Sharing_Economy\\Simulation\\Platform\\Covid\\Code 2025") # This line stores on specific directory
png("pop_density_vs_infections_by_month.png", width = 12, height = 4, units = 'in', res = 300) #This line will save the image as PNG format
a<- ggplot(data=df_avg_months_nicer, 
           aes(x=positive_new_per_week_per_1000, y=pop_density, fill=color_all)) +
  geom_point(aes(colour=color_all, size=size))  +
  scale_colour_brewer(palette="Dark2") +
  labs(title="Average Weekly New Cases vs population density", subtitle="Tanah Merah kelurahans in brown color", 
       x = "Average Weekly New Cases (per 1,000 people)",
       y = "Pop. density (people per sq. km)") + facet_wrap(~as.factor(month),scales = "free", nrow = 1) + 
  geom_hline(yintercept = mean(df_avg_months$pop_density, na.rm = TRUE),linetype="dashed", color = "red", size=1.5) +
  geom_vline(xintercept = mean(df_avg_months$avg_per_month , na.rm = TRUE),linetype="dashed", color = "red", size=1.5) +
  theme_bw(base_size = 17) +   theme(legend.position = "") +
  guides(fill=guide_legend(nrow=2)) 
a
print(a)
dev.off()
graphics.off()

## Some comments about the figure
# 1. The x axis plots the key variable: positive_new_per_week_per_1000
#      remember that this variable is the average of the 4 weeks of the month
# 2. The y axis plots the population density (e.g., people per sq. km)
# 3. The dots in green color correspond to every kelurahan
# 4. Dots in larger size correspond to kelurahans where we surveyed data
# 5.  facet_wrap() is a nice ggplot function that allows plot identical figures per group,
#       in this case our groups corresponds to the sample months
# 6. The horizontal (vertical) dashed line indicates the average value. For example,
#       since population density do not change in time, the horizontal line is always the same,
#       conversely, the vertical line changes per month because avg. new cases per week vary on time
#       We draw these lines manually with the funcitons geom_hline and geom_vline
# 7. Remember we mentioned about 4 quadrants? (I. High infection - High pop. density ...)
#      well, these quadrants correspond to the quadrants in each sub-figure
ggsave(filename = "Average Weekly New Cases vs population density.eps", 
       plot=a,family="Times",
       width = 12, height = 4, dpi = 300, units = "in") # With this line we export the figure as EPS vector format to high resolution printing






#######################################################
#######################################################
#################      PART II      ###################
#######################################################
#######################################################

#--------------------------------------------------------------
#     Bar charts with 4 scenarios: 
#                         I. High infection - High pop. density 
#                        II. Low infection - Low pop. density 
#                       III. Low infection - High pop. density 
#                        IV. High infection - Low pop. density 
#--------------------------------------------------------------

## The trick here is to add a new colum that indicates to which quadtant/group the kelurahan
# is classified every month. Note that the groups change in time because sometimes kelurahans
# have high infections/low infections

df_avg_months$quadrant1 <- ifelse(df_avg_months$positive_new_per_week_per_1000>=df_avg_months$avg_per_month &
                                     df_avg_months$pop_density >= mean(df_avg_months$pop_density, na.rm = TRUE),1,0)
df_avg_months$quadrant2 <- ifelse(df_avg_months$positive_new_per_week_per_1000>=df_avg_months$avg_per_month  &
                                     df_avg_months$pop_density <= mean(df_avg_months$pop_density, na.rm = TRUE),3,0)
df_avg_months$quadrant3 <- ifelse(df_avg_months$positive_new_per_week_per_1000 <= df_avg_months$avg_per_month  &
                                     df_avg_months$pop_density >= mean(df_avg_months$pop_density, na.rm = TRUE), 4,0)
df_avg_months$quadrant4 <- ifelse(df_avg_months$positive_new_per_week_per_1000 <= df_avg_months$avg_per_month  &
                                     df_avg_months$pop_density <= mean(df_avg_months$pop_density, na.rm = TRUE),2,0)

df_avg_months$quadrant <- df_avg_months$quadrant1 + df_avg_months$quadrant2 +
  df_avg_months$quadrant3 + df_avg_months$quadrant4
df_avg_months$quadrant1 <- NULL
df_avg_months$quadrant2 <- NULL
df_avg_months$quadrant3 <- NULL
df_avg_months$quadrant4 <- NULL

setwd("C:\\Users\\L03565094\\Dropbox\\Francisco\\Papers_SUTD\\Sharing_Economy\\Simulation\\Platform\\Covid\\Code 2025") # This line stores on specific directory
png("quadrants.png", width =12, height = 6, units = 'in', res = 300)
a <- ggplot(df_avg_months, aes(fill=as.factor(quadrant), y=quadrant, x=as.factor(month))) + 
  geom_bar(position="fill", stat="identity") + 
  labs(title="Average Weekly New Cases vs population density", 
       x = "Month",
       y = "Proportion of kelurahanes in each category") +
  scale_fill_viridis(discrete=TRUE, 
                     name = "", labels = c("I. High inf.-High pop. den.", 
                                            "II. Low inf.-Low pop. den.", 
                                            "III. Low inf.-High pop. den.", 
                                            "IV. High inf.-Low. pop. den."))+
  theme_bw(base_size = 18) + 
  theme(legend.position = "bottom")
a
print(a)
dev.off()
graphics.off()
## Some comments about the figure
# 1. The y axis displais the propotion (between 0 and 1) of kelurahants in each quadrant. 
#       For example, in April2020 more than 30% of the kelurahans were classified 
#       as IV. High infection-Low pop. density(in yellow color).
#      Recall that htis variable is the average of the 4 weeks of the month
# 2. The x axis displays the information per month. 
#       Here is important to define the levels of the factor variable,
#       otherwise it will sorts the months alphabetically (lines 208-212)
# 3. The function  scale_fill_viridis allows the use of nicer color pallet for R
ggsave(filename = "quadrants.eps", plot=a,family="Times",
       width = 12, height = 6, dpi = 300, units = "in")



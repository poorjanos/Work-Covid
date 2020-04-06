library(here)
library(dplyr)
library(lubridate)
library(ggplot2)


#########################################################################################
# Data Extraction #######################################################################
#########################################################################################

# Set JAVA_HOME, set max. memory, and load rJava library
java_version = config::get("java_version", file = "C:\\Users\\PoorJ\\Projects\\config.yml")
Sys.setenv(JAVA_HOME = java_version$JAVA_HOME)
options(java.parameters = "-Xmx2g")
library(rJava)

# Output Java version
.jinit()
print(.jcall("java/lang/System", "S", "getProperty", "java.version"))

# Load RJDBC library
library(RJDBC)

# Get credentials
datamnr <-
  config::get("datamnr", file = "C:\\Users\\PoorJ\\Projects\\config.yml")

# Create connection driver
jdbcDriver <-
  JDBC(driverClass = "oracle.jdbc.OracleDriver", classPath = "C:\\Users\\PoorJ\\Desktop\\ojdbc7.jar")

# Open connection: kontakt---------------------------------------------------------------
jdbcConnection <-
  dbConnect(
    jdbcDriver,
    url = datamnr$server,
    user = datamnr$uid,
    password = datamnr$pwd
  )

# Fetch data
 covid_query <- "SELECT   DISTINCT
           ervenyesseg as idoszak,
           termekcsoport,
           CASE WHEN hatralek_ho in ('0', '1') THEN 'N' ELSE 'I' END AS hat_2_ho,
           sum(elo_szerzodes_db) as darab
    FROM   wagnerj.covid_his_adatok
    WHERE hatralek_ho <> 'hiba'
    AND termekcsoport is not null
    group by ervenyesseg,
           termekcsoport,
           CASE WHEN hatralek_ho in ('0', '1') THEN 'N' ELSE 'I' END
ORDER BY   2, 1, 3"

covid_df <- dbGetQuery(jdbcConnection, covid_query)

# Close db connection: kontakt
dbDisconnect(jdbcConnection)



#########################################################################################
# Time Series ###########################################################################
#########################################################################################

covid_df <-
  covid_df %>% mutate(IDOSZAK = ymd_hms(IDOSZAK))

# Compute arrears rate
covid_agg <-
  covid_df %>%
  group_by(IDOSZAK, TERMEKCSOPORT) %>% 
  mutate(TOTAL = sum(DARAB)) %>% 
  mutate(HATRALAKOS_ARANY = DARAB / TOTAL) %>% 
  ungroup() %>% 
  filter(HAT_2_HO == 'I') %>% 
  select(IDOSZAK, TERMEKCSOPORT, HATRALAKOS_ARANY)
 
 
# Compute impute values  
impute <- 
  covid_agg %>% filter(IDOSZAK == as.Date('2019-11-30') | IDOSZAK == as.Date('2020-01-31')) %>%
  group_by(TERMEKCSOPORT) %>% 
  summarize(HATRALAKOS_ARANY = mean(HATRALAKOS_ARANY)) %>%
  ungroup() %>% 
  mutate(IDOSZAK = as.POSIXct('2019-12-31', tz = 'UTC')) %>% # Must set tz! unless rbind will not work
  select(IDOSZAK, TERMEKCSOPORT, HATRALAKOS_ARANY)


#attr(covid_agg$IDOSZAK, "tzone")
#attr(impute$IDOSZAK, "tzone")

#Impute
covid_agg_imp <- rbind(covid_agg, impute) %>% 
  arrange(TERMEKCSOPORT, IDOSZAK)

write.csv(covid_agg_imp, here::here("Data", "covid_agg_imp"), row.names = FALSE)



#########################################################################################
# Seasonal Decomp #######################################################################
#########################################################################################

# Get ts
lak = covid_agg_imp %>% filter(TERMEKCSOPORT == 'LAKAS')


lak_ts <- ts(lak$HATRALAKOS_ARANY, frequency = 12, start =  c(2018, 1))
plot.ts(lak_ts)

lak_ts_comp <- decompose(lak_ts)
plot(lak_ts_comp)

lak_ts_stl <-  stl(lak_ts, s.window = 'periodic')
plot(lak_ts_stl)



# Check log trends -> does not make much sense due to small changes
lak_trend <- cbind(lak[1], as.data.frame(as.vector(lak_ts_stl$time.series[,2])))
names(lak_trend) <- c('IDOSZAK', 'TREND')

ggplot(lak_trend, aes(x = IDOSZAK)) +
  geom_line(aes(y = log(TREND)))


ggplot(lak_trend, aes(x = IDOSZAK)) +
  geom_line(aes(y = TREND)) 

#########################################################################################
# Smoothing #############################################################################
#########################################################################################
# library(RcppRoll)
# 
# lak_rolling <- 
# lak %>% mutate(avg = roll_mean(HATRALAKOS_ARANY, 3, na.rm=TRUE, align="right", fill = NA))


##########################################################################################
# Push app to shinyapps.io ###############################################################
# ########################################################################################

Sys.setenv(http_proxy = proxy$http)
Sys.setenv(https_proxy = proxy$http)

rsconnect::deployApp(appName = "Covid", forceUpdate = TRUE)
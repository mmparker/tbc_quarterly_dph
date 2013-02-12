


# Strings ain't factors
options(stringsAsFactors = FALSE)


library(RODBC)


# Load the actives
plus <- odbcConnect("tbdbplus")

actives <- sqlQuery(plus, "
    SELECT local_id AS mrn, 
           person_id, 
           last_name,
           first_name,
           date_of_birth,
           date_counted,
           DatePart('q', date_counted) AS rep_qtr, 
           DatePart('yyyy', date_counted) AS rep_yr,
           reportingcountyid
    FROM Actives_List
    ORDER BY date_counted
")



# Load the county codes
cocodes <- sqlQuery(plus, "SELECT * FROM Def_county")



actco <- merge(x = actives,
               y = cocodes,
               all.x = TRUE,
               by.x = "reportingcountyid",
               by.y = "county_of_residence")


metro <- subset(actco, dmtbc_covered %in% "True")


case_counts <- ddply(metro, .var = c("rep_yr", "rep_qtr"), summarise,
      n_denver = sum(county %in% "Denver", na.rm = TRUE),
      n_other = sum(!county %in% "Denver", na.rm = TRUE)
)

subset(case_counts, rep_yr > 2010)

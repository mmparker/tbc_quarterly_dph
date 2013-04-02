


# Strings ain't factors
options(stringsAsFactors = FALSE)


library(RODBC)
library(plyr)


# Connect to TBdbPlus
plus <- odbcConnect("tbdbplus64")


# Load the encounters
enc <- sqlQuery(plus, "

	SELECT DISTINCT m.person_id, 
                    m.eval_date AS visit_date, 
                    m.visit_location,
                    m.eval_type,
                    m.staff_responsible, 
                    d.affiliation AS eval_affiliation, 
                    d.role AS eval_role
	FROM medical_eval m 
    LEFT OUTER JOIN def_staff d
	ON m.staff_responsible = d.staff_name
	WHERE (m.eval_type = 1 OR m.eval_type = 4)
		AND m.eval_date > #2007-12-31#
		AND d.affiliation = 'Denver Metro TB Clinic'
")



# Load the tx
tx <- sqlQuery(plus, "

	SELECT DISTINCT t.person_id, 
                    t.treatment_date AS visit_date, 
                    t.completed, 
                    t.dispense_type,
                    t.staff, 
                    d.affiliation AS tx_affiliation, 
                    d.role AS tx_role
	FROM drug_treatment t 
    LEFT OUTER JOIN def_staff d
	ON t.staff = d.staff_name
	WHERE t.completed in ('Completed', 'Missed')
		AND t.treatment_date > #2007-12-31#
		AND d.affiliation = 'Denver Metro TB Clinic'

")


# MRN crosswalk
mrns <- sqlQuery(plus, "

    SELECT idnumber AS person_id, local_id AS mrn
    FROM Person

")


odbcClose(plus)


# Merge into unified visits
visits <- merge(x = enc,
                y = tx,
                by = c("person_id", "visit_date"),
                all = TRUE)



# Add month/qtr/year variables for aggregation
visits$visit_mon <- as.character(format(as.Date(visits$visit_date), "%m"))

visits$visit_qtr <- NA
visits$visit_qtr[visits$visit_mon %in% c("01", "02", "03")] <- "1"
visits$visit_qtr[visits$visit_mon %in% c("04", "05", "06")] <- "2"
visits$visit_qtr[visits$visit_mon %in% c("07", "08", "09")] <- "3"
visits$visit_qtr[visits$visit_mon %in% c("10", "11", "12")] <- "4"

visits$visit_yr <- as.character(format(as.Date(visits$visit_date), "%Y"))


count(visits, c("visit_yr", "visit_qtr"))



# 2012 visits by staff
twelve <- subset(visits, visit_yr %in% "2012")
count(twelve, c("visit_qtr"))

bystaff <- arrange(count(twelve, c("staff_responsible", "staff")), desc(freq))
byrole <- arrange(count(twelve, vars = c("eval_role", "tx_role")), desc(freq))

twelve$staffguess <- twelve$staff_responsible
twelve$staffguess[is.na(twelve$staffguess)] <- 
    twelve$staff[is.na(twelve$staffguess)]

table(twelve$staffguess, exclude = NULL)







# Pull in the LifeLink visits for comparison
dhdw <- odbcConnect("dhdw64")

ll_visits <- sqlQuery(dhdw, "

SELECT pid.IdVal AS mrn, a.PtObjId, a.ApptObjId, a.DeptMne AS Clinic,
       a.LocMne AS Location, a.SchedDtime AS Appt_sched_dtime, 
       a.StrDtime AS Appt_dtime, CAST(a.StrDtime AS date) AS appt_date,
       a.ActvNo, actv.ActvMne AS Actv_desc, a.StsCd, stsmstr.StsDesc, 
       a.PrimResObjId AS Appt_Resource_ID, res.ResDesc AS Appt_Resource_Name,
       res.ResMne AS Res_Code, res.ResCatCd, 
       case when a.NoShowInd = '1' then 'No Show'
            else 'Show'
            end AS NoShowInd,
       case when a.AtnInd = '1' then 'Attend'
            else 'Not Attend'
            end AS AtnInd,
       case when a.CmpltInd = '1' then 'Completed'
            else 'Not completed'
            end AS CmpltInd,
       case when a.ConfInd = '1' then 'Confirmed'
            else 'Not Confirmed'
            end AS ConfInd,
       case when a.DNKAInd = 'Y' then 'No Show'
            else 'Show'
            end AS DNKAInd,
       a.RsnText, a.DNKARsnCd, a.DNKARsnText

FROM dhdcdtw.smsdss.s2_ApptVstPtV a with (nolock)

LEFT JOIN dhdcdtw.smsdss.S2_ActvMstrV actv with (nolock)
    ON a.ActvNo = actv.ActvNo
    AND a.DeptMne = actv.DeptMne

LEFT JOIN dhdcdtw.smsdss.S2_ApptStsMstrV stsmstr with (nolock)
    ON a.StsCd = stsmstr.StsCd 

LEFT JOIN dhdcdtw.smsdss.S2_ResMstrV res with (nolock) 
    ON a.PrimResObjId = res.ResObjId

LEFT JOIN dhdcdtw.smsdss.S2_PtIdV pid with (nolock) 
    ON a.ptobjid = pid.ptobjid 
    AND pid.IDTypeMNE = 'MRN'

WHERE  a.StrDtime >= '2012-01-01'
    AND a.StrDtime < '2013-01-01'
    AND LocMne = 'PAV H TBC'
	AND a.CmpltInd = '1'
ORDER BY pid.IDVal, a.StrDtime

")


odbcClose(dhdw)



# How many visits by provider?
count(ll_visits, vars = "Appt_Resource_Name")



# Merge onto the TBdb visits, inshallah

# Add MRNs to TBdb
tb_visits <- merge(x = subset(visits, visit_yr %in% c(2011, 2012)),
                   y = mrns,
                   by = "person_id",
                   all.x = TRUE)

tb_visits$in_tbdb <- TRUE
ll_visits$in_ll <- TRUE

tbll <- merge(x = tb_visits,
              y = subset(ll_visits, 
                         select = c("mrn", "Clinic", "Location", "appt_date",
                                    "Actv_desc", "StsDesc", 
                                    "Appt_Resource_Name", "ResCatCd",
                                    "NoShowInd", "AtnInd", "CmpltInd",
                                    "in_ll")),
              by.x = c("mrn", "visit_date"),
              by.y = c("mrn", "appt_date"),
              all = TRUE)


count(tbll, vars = c("in_tbdb", "in_ll"))
count(tbll, vars = c("Appt_Resource_Name", "in_tbdb"))



# Write out a list of visits that aren't in TBdb
notintbdb <- subset(tbll, 
                    subset = in_tbdb %in% NA & 
                             !Appt_Resource_Name %in% c("ANGELICA CORDOVA HCP",
                                                        "Leslie Sotelo"),
                    select = c("mrn", "visit_date", "Actv_desc",
                               "Appt_Resource_Name", "CmpltInd")
)

write.csv(notintbdb, "Visits in LifeLink but not TBdb.csv", row.names = FALSE)


# Call 'em clinic vs. outreach
tbll$site <- NA
tbll$site[tbll$in_ll %in% TRUE] <- "Clinic"

table(tbll$site, exclude = NULL)

count(tbll, vars = c("site", "eval_role", "tx_role"))


# Make a placeholder for the clinic-based roles
clinicbased <- c("Physician", "HCP", "Nurse", "Administrative") 


# The CDC PHAs only did outreach
tbll$site[is.na(tbll$site) & (
          tbll$eval_role %in% "CDC PHA" | 
          tbll$tx_role %in% "CDC PHA")] <- "Outreach"




# If the person sees a physician, HCP, nurse, or administrative person,
# that's obviously in-clinic
tbll$site[is.na(tbll$site) & 
          tbll$eval_role %in% clinicbased] <- "Clinic"

tbll$site[is.na(tbll$site) & 
          tbll$tx_role %in% clinicbased] <- "Clinic"




# If it's a DIS person AND NOT a clinic-type, it's Outreach
tbll$site[is.na(tbll$site) & 
          tbll$eval_role %in% "DIS" &
          !tbll$tx_role %in% clinicbased] <- "Outreach"

tbll$site[is.na(tbll$site) & 
          !tbll$eval_role %in% clinicbased &
          tbll$tx_role %in% "DIS"] <- "Outreach"


# "Research" role in this context basically means Juanita Lovato - 
# she did outreach for a while
tbll$site[is.na(tbll$site) & (
          tbll$eval_role %in% "Research" | 
          tbll$tx_role %in% "Research")] <- "Outreach"



count(tbll, vars = c("site", "eval_role", "tx_role"))
count(tbll, vars = "site")



# Tally by year and quarter - without Sonia
visagg <- dcast(count(tbll, vars = c("visit_yr", "visit_qtr", "site")),
                visit_yr + visit_qtr ~ site)

visagg$total <- with(visagg, Clinic + Outreach)
visagg

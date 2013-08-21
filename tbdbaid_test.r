





# Make sure the tbdbaid equivalent function returns the same results
actives_tbdbaid <- tbdbaid::query_actives(
    start_date = as.Date(paste(as.numeric(format(Sys.Date(), "%Y")) - 10,
                               "-01-01", 
                               sep = "")),
    stop_date = last.qtr$end.date)



nrow(actives)
nrow(actives_tbdbaid)
all(actives$person_id %in% actives_tbdbaid$person_id)
identical(actives, actives_tbdbaid)






# Same with visits
visits_tbdbaid <- tbdbaid::query_visits(start_date = first.qtr$start.date,
                                        stop_date = last.qtr$end.date)

nrow(visits)
nrow(visits_tbdbaid)
all(visits$person_id %in% visits_tbdbaid$person_id)
identical(visits, visits_tbdbaid)




# Test results
tests_tbdbaid <- tbdbaid::query_tests(start_date = first.qtr$start.date,
                                      stop_date = last.qtr$end.date)

nrow(tests)
nrow(tests_tbdbaid)
all(tests$person_id %in% tests_tbdbaid$person_id)
identical(tests, tests_tbdbaid)




# Finally, treatment plans
plan_starts_tbdbaid <- tbdbaid::query_tx_plans(
    start_date = get_quarter(Date = Sys.Date(), offset = -9)$start.date,
    stop_date = get_quarter(Date = Sys.Date(), offset = -1)$end.date)


nrow(plan_starts)
nrow(plan_starts_tbdbaid)
all(plan_starts$person_id %in% plan_starts_tbdbaid$person_id)
identical(plan_starts, plan_starts_tbdbaid)


min_conc <- 10
molecule <- "Olivetol"

molecule_filter <- c(molecule, toupper(molecule)) %>%
            jsonlite::toJSON(pretty = TRUE)


query <- str_interp('{
                                "data": { "$elemMatch" :
                                            {
                                            "Concentration" : {"$gt" : ${min_conc}},
                                            "Molecule" : { "$regex" : "${molecule}", "$options": "i" }
                                            }
                                }
                            }')
    match <- str_interp('{ "$match": ${query} }')
    project1 <- str_interp('{
                                "$project" :
                                    {"data" :
                                        {"$filter":
                                            {"input" :
                                                "$data",
                                                "cond":
                                                    {"$and": [
                                                        {"$gt":["$$this.Concentration",${min_conc}]},
                                                        {"$in" : ["$$this.Molecule", ${molecule_filter}  ]}
                                                    ]}
                                            }
                                        },
                                    "xml" : "$name",
                                    "date_created" : 1
                                    }
                        }')

    unwind <- str_interp('{ "$unwind" : "$data"}')
    project2 <- str_interp('{
                                "$project" :
                                    {"data.Strain":
                                        {"$toUpper": "$data.Strain"},
                                    "xml" : 1,
                                    "date_created" : 1,
                                    "data.Concentration" : 1,
                                    "data.sampleid": 1,
                                    "data.Name" : 1
                                    }
                                    }')
    group <- str_interp('{
                                "$group": {
                                    "_id": {
                                            "strain": "$data.Strain",
                                            "date_created" : "$date_created",
                                            "xml" : "$xml"
                                            },
                                    "count": {"$sum" : 1},
                                    "mean": {"$avg": "$data.Concentration"},
                                    "samples_data" : {"$push": {
                                                        "concentration": "$data.Concentration",
                                                        "id": "$data.sampleid",
                                                        "name": "$data.Name"
                                                        }}
                                }
                                    }')



test <- do_pipeline(db = ms_db, match, project1, unwind, project2, group) %>% head()
test
do_pipeline(db = ms_db, match, project1, unwind, project2) %>% head()



    aggregate_pipeline <- function(db, ...) {
    pipeline <- paste(..., sep = ",")
    pipeline <- str_interp('[${pipeline}]')

    res <- safe_prettify(pipeline)
    if (is.null(res$error)) {
        db$aggregate(pipeline)
    } else {
        res$error$message
    }
    }

        safe_prettify <- safely(jsonlite::prettify)
        safe_prettify('{"date_created" : 1, "name" : 1}')

ms_db$find('{}', '{"date_created" : 1, "name" : 1}')


ms_db$aggregate(pipeline = str_interp('[
            {
                "$match": ${query}
            },
            {
                "$unwind": "$data"
            },
            {
                "$group" : {
                    "_id" : {"Strain": "$data.Strain", "Doc": "$name"},
                    "mean": {"$avg" : "$data.Concentration"},
                    "nsamples": {"$size" : "$data"}
                }
            }
]'))



# Sans unind
test <- ms_db$aggregate(pipeline = str_interp('[
                                    {
                                      "$match": ${query}
                                      }
                                      ]'))




ms_db$find(
    query = str_interp(
        '{
        "data": { "$elemMatch" :
        {
        "Concentration" : {"$gt" : 10},
        "Molecule" : { "$regex" : "olivetol", "$options": "i" }
        }
        }
        }'
            ),
    fields = '{"_id": 0, "name" : 1, "data" : 1}')


ms_db$find(
    query = str_interp(
        '{"name" :  "2018-02-06 18-02-07_quandata_olivetolscreening.xml",
        "data": { "$elemMatch" :
        {
        "Concentration" : {"$gt" : 10},
        "Molecule" : { "$regex" : "olivetol", "$options": "i" }
        }
        }
        }'
            ),
    fields = '{"_id": 0, "name" : 1, "data" : 1}')

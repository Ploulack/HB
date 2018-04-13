library(tidyverse); library(mongolite)
source("helpers/mongo_helpers.R")

search_mol_by_min_conc <- function(db, molecule, min_conc, with_samples = FALSE) {
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
                                    "data.Name" : 1,
                                    "data.Molecule" : 1}
                                    }')
    group <- str_interp('{
                            "$group": {
                                "_id": {
                                    "strain": "$data.Strain",
                                    "date_created" : "$date_created",
                                    "xml" : "$xml"
                                },
                                "count": {"$sum" : 1},
                                "mean": {"$avg": "$data.Concentration"}
                            }
                        }')
    if (with_samples) {
        res <- aggregate_pipeline(db, match, project1, unwind, project2)
    } else {
        res <- aggregate_pipeline(db, match, project1, unwind, project2, group)
    }
    res
}
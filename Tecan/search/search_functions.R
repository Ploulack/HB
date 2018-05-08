library(tidyverse); library(mongolite)
source("helpers/mongo_helpers.R")

search_mol_by_min_conc <- function(db, molecule, min_conc, max_conc, tags, with_samples = FALSE) {
     molecule_filter <- c(molecule, toupper(molecule)) %>%
          jsonlite::toJSON(pretty = TRUE)
     tags_filter <- tags %>%
          jsonlite::toJSON()
     tags_size <- tags %>% length()

     query_start <- str_interp('{
                                "data": { "$elemMatch" :
                                            {
                                            "Concentration" : {"$gt" : ${min_conc}, "$lt" : ${max_conc}},
                                            "Molecule" : { "$regex" : "${molecule}", "$options": "i" }
                                            ')
     query_tags <- str_interp(',
                                            "Tags" : {"$all": ${tags_filter}}')

     query_end <- str_interp('}}}')

     project_start <- str_interp('{
                                "$project" :
                                    {"data" :
                                        {"$filter":
                                            {"input" :
                                                "$data",
                                                "cond":
                                                    {"$and": [
                                                       {"$gt":[ "$$this.Concentration",${min_conc} ]},
                                                       {"$lt":[ "$$this.Concentration",${max_conc} ]},
                                                       {"$in" : [ "$$this.Molecule", ${molecule_filter} ]}
        ')

tag_search <- str_interp(',
                                                       {"$gte": [ {"$size": "$$this.Tags"}, ${tags_size}  ]},
                                                       {"$gte": [
                                                            {"$sum":
                                                                 {"$map" : {
                                                                      "input" : "$$this.Tags",
                                                                      "as" : "tag",
                                                                      "in" : {"$cond":[
                                                                                {"$in" : ["$$tag", ${tags_filter}] },
                                                                                1,0]
                                                                      }
                                                                 }}
                                                            },
                                                            ${tags_size}
                                                       ]}
')

project_end <- str_interp('
                                                  ]}
                                           }
                                        },
                                    "xml" : "$name",
                                    "date_created" : 1
                                    }
                        }')

     if (is.null(tags)) {

          project1 <- paste0(project_start, project_end)
          query <- paste0(query_start, query_end)
     } else {
          project1 <- paste0(project_start, tag_search, project_end)
          query <- paste0(query_start, query_tags, query_end)
     }

     match <- str_interp('{ "$match": ${query} }')


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
                                   "data.Molecule" : 1,
                                   "data.Tags" : 1}
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

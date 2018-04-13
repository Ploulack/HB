#Note strategy :
# faire d'abord
# aggregate_pipeline(db, match, project1, unwind, project2, group)
# puis si count total < X faire
# aggregate_pipeline(db, match, project1, unwind, project2)


db_ms$find('{"data.Tags" : "olivetol", "data.Concentration": { "$exists" : true}}')
db_ms$find('{"data.Strain" : "HB189"}', '{"name" : 1, "data" : {"$slice" : 1}}')




db_ms$find('{ "data": { "$elemMatch" : { "Concentration" : {"$gt" : 5000},
                "Molecule" : { "$regex" : "[Oo]livetol", "$options": "" } } } }',
           '{"name" : 1}')

ms_db$find(str_interp('{ "data": { "$elemMatch" : { "Concentration" : {"$gt" : 10},
                "Molecule" : { "$regex" : "CBD", "$options": "i" } } } }'),
           '{"name" : 1}')

ms_db$find(str_interp('{ "data": { "$elemMatch" : { "Concentration" : {"$gt" : ${input$min_concentration}},
                "Molecule" : { "$regex" : "${input$search_molecules}", "$options": "" } } } }'),
           '{"name" : 1}')

test <- db_ms$find('{"data.Concentration" : {"$gt" : 100},
           "data.Molecule": "olivetol"}')



test$data[[1]] %>%
    filter(Concentration > 100)


db_ms$find('{"data.Concentration" : {"$gt" : 100}}', '{"name" : 1}')
db_ms$find('{"data.Concentration" : }', '{"name" : 1}')


test <- db_ms$find('{"data.Concentration" : {"$gt" : 100}}')

test$data[[2]] %>% filter(Concentration > 100)

test$data[[2]] %>% group_by(Molecule) %>% summarise(max = max(Concentration, na.rm = TRUE))
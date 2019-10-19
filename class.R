
setClass("Group",
         slots = c(
                 name = "character",
                 rule = "character",
                 weight = "numeric",
                 teams = "character",
                 schedule = "list",
                 scores = "list",
                 table = "data.frame",
                 round = "integer"
                 )
         )


setGeneric("setFixture", function(x) standardGeneric("setFixture"))

setGeneric("nextRound", function(x) standardGeneric("nextRound"))

setGeneric("finishAllrounds", function(x) standardGeneric("finishAllrounds"))



setMethod("setFixture", "Group", function(x) {
        x@schedule <- robin(x@teams, x@rule)
        
        x@scores <- vector(mode = "list", length = length(x@schedule))
        
        x@round <- 0L
        x@table <- tibble(Team = x@teams,
                          Pld = 0,
                          W = 0,
                          D = 0,
                          L = 0,
                          GF = 0,
                          GA = 0,
                          GD = 0,
                          Pts = 0)
        return(x)
})

setMethod("nextRound", "Group", function(x) {
        
        if (x@round >= length(x@schedule)) {
                
                message("Already finished")
                
                print(x@table %>% arrange(desc(Pts), desc(GD), desc(GF)))
                
                return(x)
        }
        
        n <- x@round + 1
        
        x@scores[[n]] <- vector(mode = "list", length = length(x@schedule[[n]]))
        
        for (i in seq_along(x@schedule[[n]])) {
                
                x@scores[[n]][[i]] <- run_match(pair = x@schedule[[n]][[i]], 
                                               neutral = if (x@rule == "home_away") FALSE else TRUE,
                                               weight = x@weight)
                
                ts <- x@schedule[[n]][[i]]
                ss <- x@scores[[n]][[i]]
                
                # if ("EMPTY" %in% ts) {
                #         print(paste(ts[!ts %in% "EMPTY"], "BYE"))
                # } else {
                #         print(paste(ts[1], ss[1], "--", ss[2], ts[2]))
                # }
                
                x@table <- add_score(x@table, ts, ss)
        }
        
        if (n == length(x@schedule)) {
                
                print(x@table %>% arrange(desc(Pts), desc(GD), desc(GF)))
                
        }
        
        x@round <- x@round + 1L
        
        return(x)
        
})


setMethod("finishAllrounds", "Group", function(x) {
        
        if (x@round >= length(x@schedule)) {
                
                message("Already finished")
                return(x)
        }
        
        for (i in (x@round+1):length(x@schedule)) {
                
                x <- nextRound(x)
        }

        return(x)
        
})


setClass("Continent",
         slots = c(
                 name = "character",
                 rule = "character",
                 weight = "numeric",
                 group_number = "numeric",
                 q_number = "numeric",
                 groups = "list",
                 q_teams = "character"
         )
)

setGeneric("setGroups", function(x) standardGeneric("setGroups"))



setMethod("setGroups", "Continent", function(x) {
        
        teams <- rank_2018 %>%
                filter(confederation == x@name) %>%
                arrange(rank) %>%
                `[[`("country_full")
        
        n <- x@group_number
        
        pot <- set_pot(teams, n)
        
        grouping <- set_group(pot)
        
        for (i in 1:n) {
                
                x@groups[[i]] <- new("Group", name = LETTERS[i], rule = x@rule, weight = x@weight,
                                     teams = grouping[[i]])
                
                x@groups[[i]] <- setFixture(x@groups[[i]])
        }
        
        return(x)
        
})


setMethod("nextRound", "Continent", function(x) {
        
        for (i in 1:x@group_number) {
                
                x@groups[[i]] <- nextRound(x@groups[[i]])
        }
        
        return(x)
        
})

setMethod("finishAllrounds", "Continent", function(x) {
        
        rank <- list()
        
        for (i in 1:x@group_number) {
                
                x@groups[[i]] <- finishAllrounds(x@groups[[i]])
                
                rank[[i]] <- x@groups[[i]]@table %>% 
                                arrange(desc(Pts), desc(GD), desc(GF)) %>%
                                `[[`("Team")
        }
        
        x@q_teams <- unlist(map(.x = rank, .f = head, n = x@q_number))
        
        print(x@q_teams)
        
        return(x)
        
})


setClass("World",
         slots = c(
                 rule = "character",
                 weight = "numeric",
                 groups = "list",
                 q_teams = "character",
                 confs = "list",
                 group_number = "numeric",
                 q_number = "numeric",
                 teams = "character",
                 final = "list"
         )
)

setGeneric("qualify", function(x) standardGeneric("qualify"))

setGeneric("runGroups", function(x) standardGeneric("runGroups"))

setGeneric("runPlayoff", function(x) standardGeneric("runPlayoff"))


setMethod("qualify", "World", function(x) {
        
        for (i in seq_along(rules$conf)) {
                
                x@confs[[i]] <- new("Continent", name = rules$conf[i], rule = "home_away", weight = 1, 
                                    group_number = rules$group_number[i], q_number = rules$q_number[i])
                
                x@confs[[i]] <- setGroups(x@confs[[i]])
                
                x@confs[[i]] <- finishAllrounds(x@confs[[i]])
                
                x@teams <- append(x@teams, x@confs[[i]]@q_teams)

        }
        
        print(x@teams)
        
        return(x)
        
})

setMethod("setGroups", "World", function(x) {
        
        teams <- rank_2018 %>%
                filter(country_full %in% x@teams) %>%
                arrange(rank) %>%
                `[[`("country_full")
        
        n <- x@group_number
        
        pot <- set_pot(teams, n)
        
        grouping <- set_group(pot)
        
        for (i in 1:n) {
                
                x@groups[[i]] <- new("Group", name = LETTERS[i], rule = x@rule, weight = x@weight,
                                     teams = grouping[[i]])
                
                x@groups[[i]] <- setFixture(x@groups[[i]])
        }
        
        return(x)
        
})


setMethod("runGroups", "World", function(x) {

        rank <- list()
        
        for (i in 1:x@group_number) {
                
                x@groups[[i]] <- finishAllrounds(x@groups[[i]])
                
                rank[[i]] <- x@groups[[i]]@table %>% 
                        arrange(desc(Pts), desc(GD), desc(GF)) %>%
                        `[[`("Team")
        }
        
        x@q_teams <- unlist(map(.x = rank, .f = head, n = x@q_number))
        
        print(x@q_teams)
        
        return(x)
        
})

setMethod("runPlayoff", "World", function(x) {
        
        x@final <- run_play_off(x@q_teams)
        
        return(x)
        
})

world <- new("World", rule = "tournament", weight = 2, group_number = 8, q_number = 2)

world <- qualify(world)

world <- setGroups(world)

world <- runGroups(world)

world <- runPlayoff(world)









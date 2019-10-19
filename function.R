

ran_cty <- function(country) {
        if (country %in% rank_2018$country_full) {
                return(filter(rank_2018, country_full == country)$rank)
        } else if (country == "EMPTY") {
                return(-1)
        } else {
                stop("Country not found")
        }
}

score_model <- function(rank, diff, home, weight) {
        
        l <- 1.055 + 2.122e-03 * rank + 3.052e-01 * home + 6.085e-02 *weight + 1.212e-02 * diff + 3.223e-05 * diff^2
        return(l)
}

score_model(40, -40, 0, 0)

run_match <- function(pair, neutral = FALSE, weight = 0, draw = TRUE) {
        
        if (length(pair) != 2) {
                stop("Please input correct number of countries")
                
        } else if ("EMPTY" %in% pair) {
                return("EMPTY")
                
        } else {
                
                r <- map_dbl(.x = pair, .f = ran_cty)
                diff <- c(r[2] - r[1], r[1] - r[2])
                home <- if(neutral) c(0,0) else c(1,-1)
                weight <- rep(weight, 2)
                
                l <- pmap_dbl(.l = list(r, diff, home, weight), .f = score_model)
                
                s <- map_int(.x = l, .f = rpois, n = 1)
                
                if (!draw & s[1] == s[2]) {

                        s[1] <- sample(c(0,1))[1]
                        s[2] <- 1 - s[1]
                } 
                
                return(s)
        }
}

run_match(c("Germany","Brazil"), neutral = T, draw = F)


run_play_off <- function(teams) {
        
        if (log2(length(teams)) - floor(log2(length(teams))) != 0) {
                
                stop("Incorrect number of teams")
        }
        
        r <- log2(length(teams))
        
        l <- vector(mode = "list", length = r + 1)
        
        l[[1]] <- teams
        
        for (i in 1:r) {
                
                for (j in 1:(length(l[[i]])/2)) {
                        
                        t_a <- l[[i]][j]
                        t_b <- l[[i]][length(l[[i]]) + 1 - j]
                        
                        s <- run_match(c(t_a, t_b), neutral = TRUE, weight = 2, draw = FALSE)
                        
                        print(paste(t_a, s[1], "--", s[2], t_b))
                        
                        l[[i+1]][j] <- if (s[1] > s[2]) t_a else t_b
                        
                }
                print("========")
                
        }
        return(l)
}


run_play_off(world@q_teams)

run_match(c("China PR", "EMPTY"))

rotate_list <- function(l1, l2) {
        
        if (length(l1) != length(l2)) {
                stop("Length unequal")
        } else {
                
                n <- length(l1)
                
                r1 <- vector(mode = "character", length = n)
                r2 <- vector(mode = "character", length = n)
                
                r1[1] <- l1[1]
                r2[n] <- l1[n]
                r1[2:n] <- c(l2[1],l1[2:(n-1)])
                r2[1:(n-1)] <- l2[2:n]
                
                return(list(r1, r2))
                
        }
}
        
        
        
a <- c("A","B","C","D")
b <- c("E","F","G","H")

rotate_list(a,b)


robin <- function(teams, rule = "tournament") {
        
        if (length(teams) %% 2 == 1) {
                teams <- append(teams, "EMPTY")
        }
        
        n <- length(teams)
        
        sch <- list()

        l1 <- teams[1:(n/2)]
        l2 <- teams[(n/2+1):n]
        
        for (i in seq_along(1:(n-1))) {
                
                sch[[i]] <- list()
                
                for (j in seq_along(l1)) {
                        
                        sch[[i]][[j]] <- c(l1[j], l2[j])
                }
                
                r <- suppressWarnings(rotate_list(l1, l2))
                
                l1 <- r[[1]]
                l2 <- r[[2]]
        }
        
        if (rule == "home_away") {
                
                sch_2 <- list()
                
                for (i in seq_along(1:(n-1))) {
                        
                        sch_2[[i]] <- list()
                        
                        for (j in seq_along(l1)) {
                                sch_2[[i]][[j]] <- rev(sch[[i]][[j]])
                        }
                }
                
                sch <- append(sch, sch_2)
        }
        
        return(sch)
}

robin(a, rule = "home_away")


add_score <- function(table, teams, scores) {
        
        if (scores[1] == "EMPTY") {
                return(table)
        }
        
        if (scores[1] > scores[2]) {
                
                table %<>%
                        mutate(W = ifelse(Team == teams[1], W + 1, W),
                               L = ifelse(Team == teams[2], L + 1, L),
                               Pts = ifelse(Team == teams[1], Pts + 3, Pts))
                
        } else if (scores[1] == scores[2]) {
                
                table %<>%
                        mutate(D = ifelse(Team == teams[1], D + 1, D),
                               D = ifelse(Team == teams[2], D + 1, D),
                               Pts = ifelse(Team == teams[1], Pts + 1, Pts),
                               Pts = ifelse(Team == teams[2], Pts + 1, Pts))
                
        } else {
                table %<>%
                        mutate(W = ifelse(Team == teams[2], W + 1, W),
                               L = ifelse(Team == teams[1], L + 1, L),
                               Pts = ifelse(Team == teams[2], Pts + 3, Pts))
                
        }
        
        table %<>%
                mutate(Pld = ifelse(Team == teams[1], Pld + 1, Pld),
                       Pld = ifelse(Team == teams[2], Pld + 1, Pld),
                       GF = ifelse(Team == teams[1], GF + scores[1], GF),
                       GF = ifelse(Team == teams[2], GF + scores[2], GF),
                       GA = ifelse(Team == teams[1], GA + scores[2], GA),
                       GA = ifelse(Team == teams[2], GA + scores[1], GA),
                       GD = ifelse(Team == teams[1], GD + scores[1] - scores[2], GD),
                       GD = ifelse(Team == teams[2], GD + scores[2] - scores[1], GD))
        
        return(table)
} 


add_score(test, teams = group_a@schedule[[1]][[1]], scores = group_a@scores[[1]][[1]])


set_pot <- function(teams, n) {
        
        a <- length(teams)
        
        m <- ceiling(a / n)
        
        l <- vector(mode = "list", length = m)
        
        for (i in 1:(m - 1)) {
                l[[i]] <- teams[((i-1) * n + 1):(i * n)]
        }
        l[[m]] <- teams[(i*n + 1):a]
        
        return(l)
}



set_group <- function(pot) {
        
        n <- length(pot[[1]])
        
        l <- map(.x = pot, .f = sample)
        
        g <- vector(mode = "list", length = n)
        
        for (i in 1:n) {
                g[[i]] <- unlist(map(l, i))
        }
        return(g)
}
        
rules <- list()

rules$conf <- unique(rank_2018$confederation)
rules$group_number <- c(8, 1, 4, 6, 6, 1)
rules$q_number <- c(2, 7, 1, 1, 1, 1)

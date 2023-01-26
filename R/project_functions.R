# Functions
plover_date_convert <- 
  function(df, input = "mdd"){
    if(input == "mdd"){
      
      if(sum(grepl(paste(c("date", "alive", "manip"), collapse = "|"), names(df))) > 1){
        df[, which(grepl(paste(c("date", "alive", "manip"), collapse = "|"), names(df)))] <- 
          lapply(df[, which(grepl(paste(c("date", "alive", "manip"), collapse = "|"), names(df)))],
                 function(x) 
                   paste(df$year, 
                         ifelse(nchar(x) == 3, 
                                substring(x, first = 1, last = 1), 
                                substring(x, first = 1, last = 2)), 
                         ifelse(nchar(x) == 3, 
                                substring(x, first = 2, last = 3), 
                                substring(x, first = 3, last = 4)), 
                         sep = "-"))
        df[, which(grepl(paste(c("date", "alive", "manip"), collapse = "|"), names(df)))] <-
          lapply(df[, which(grepl(paste(c("date", "alive", "manip"), collapse = "|"), names(df)))],
                 function(x) as.Date(x, format = "%Y-%m-%d"))
      }
      else{
        df[, which(grepl(paste(c("date", "alive", "manip"), collapse = "|"), names(df)))] <- 
          paste(df$year, 
                ifelse(nchar(df[, which(grepl(paste(c("date", "alive", "manip"), collapse = "|"), names(df)))]) == 3, 
                       substring(df[, which(grepl(paste(c("date", "alive", "manip"), collapse = "|"), names(df)))], first = 1, last = 1), 
                       substring(df[, which(grepl(paste(c("date", "alive", "manip"), collapse = "|"), names(df)))], first = 1, last = 2)), 
                ifelse(nchar(df[, which(grepl(paste(c("date", "alive", "manip"), collapse = "|"), names(df)))]) == 3, 
                       substring(df[, which(grepl(paste(c("date", "alive", "manip"), collapse = "|"), names(df)))], first = 2, last = 3), 
                       substring(df[, which(grepl(paste(c("date", "alive", "manip"), collapse = "|"), names(df)))], first = 3, last = 4)), 
                sep = "-")
        df[, which(grepl(paste(c("date", "alive", "manip"), collapse = "|"), names(df)))] <- 
          as.Date(df[, which(grepl(paste(c("date", "alive", "manip"), collapse = "|"), names(df)))], format = "%Y-%m-%d")
      }
    }
    if(input == "Rdate"){
      if(sum(grepl(paste(c("date", "alive", "manip"), collapse = "|"), names(df))) > 1){
        df[, which(grepl(paste(c("date", "alive", "manip"), collapse = "|"), names(df)))] <- 
          lapply(df[, which(grepl(paste(c("date", "alive", "manip"), collapse = "|"), names(df)))],
                 function(x) 
                   as.Date(x, origin = "1970-01-01"))
      }
      else{
        df[, which(grepl(paste(c("date", "alive", "manip"), collapse = "|"), names(df)))] <- 
          as.Date(df[, which(grepl(paste(c("date", "alive", "manip"), collapse = "|"), names(df)))], origin = "1970-01-01")
      }
    }
    return(df)
  }

# function that does the opposite of "%in%"
`%!in%` = Negate(`%in%`)

## PS 5625 - Applied Statistical Programming
## Problem Set 2
## Author: Luwei Ying
## Tasks: 1 - Create benford()
##        2 - Create print.benfords()


rm(list = ls())

# Task1 - Create benford.violations()

Benford.violations<-function (x, type = "b"){
  props<-table(factor(substr(as.character(x),1,1), levels = 1:9))/length(x)
  if(type == "m"){
    m <- max(props-log(1+1/c(1:9),10))
    return(list(props, m=m))
  }
  if(type == "d"){
    d <- sqrt(sum((props-log(1+1/c(1:9),10))^2))
    return(list(props, d=d))
  }
  if(type == "b"){
    m <- max(props-log(1+1/c(1:9),10))
    d <- sqrt(sum((props-log(1+1/c(1:9),10))^2))
    return(list(props, m=m, d=d))
  }
}

# - test:
set.seed(268)

Benford.violations(sample(83974,735))
Benford.violations(seq(390:9902), "m")
Benford.violations(rep(834,1298),"d")


#Task2 - Create print.benfords()
print.benfords <- function(x, type = "b"){
  imput <- Benford.violations(x=x, type=type)
  
  if(type == "m"){
    M <- imput$m
    M <- ifelse(M < 0.851, M,
                ifelse(M < 0.967, M <- paste0(M, '*'),
                       ifelse(M < 1.212, M <- paste0(M, '**'),
                              M <- paste0(M, '***'))))
  }
  if(type == "d"){
    D <- imput$d
    D <- ifelse(D < 1.212, D, 
                ifelse(D < 1.330, D <- paste0(D, '*'),
                       ifelse(D < 1.569, D <- paste0(D, '**'),
                              D <- paste0(D, '***'))))
  }
  if(type == "b"){
    M <- imput$m
    M <- ifelse(M < 0.851, M,
                ifelse(M < 0.967, M <- paste0(M, '*'),
                       ifelse(M < 1.212, M <- paste0(M, '**'),
                              M <- paste0(M, '***'))))
    
    D <- imput$d
    D <- ifelse(D < 1.212, D, 
                ifelse(D < 1.330, D <- paste0(D, '*'),
                       ifelse(D < 1.569, D <- paste0(D, '**'),
                              D <- paste0(D, '***'))))
  }
  
#Make the Table:  
  title <- "Benford's Law Test Statistics\n\n"
  leemis_label <- "Leemis' m\n"
  CG_D_label <- "Cho-Gains' d\n"
  dividing_line <- "_____________________________\n"
  legend <- "* signifies p<0.10\n ** signifies p<0.05\n *** signifies p<0.01"

  if(type=="m"){
    cat(title, leemis_label,
        M,"\n",
        dividing_line,legend,"\n\n")
  }
  if(type=="d"){
    cat(title, CG_D_label,
        D,"\n",
        dividing_line,legend,"\n\n")
  }
  if(type=="b"){
    cat(title, leemis_label,
        M,"\n\n",
        CG_D_label,
        D,"\n",
        dividing_line,legend,"\n\n")
  }
}  

# - test:
print.benfords(sample(23454,3435), type="b") 
print.benfords(seq(390:9902), type="m") 
print.benfords(rep(834,1298), type="d") 

#Task3 - Create print.benfords()
save.benfords <- function(x, type="b", file_name="Benfords_Law.csv"){
  sink(file_name)
  print.benfords(x)
  sink()
}

# - test:
save.benfords(sample(23454,3435), type="b")
save.benfords(seq(390:9902), "m", "Another test.csv")

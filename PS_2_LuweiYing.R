## PS 5625 - Applied Statistical Programming
## Problem Set 2
## Author: Luwei Ying
## Tasks: 1 - Create Benford.violations()
##        2 - Create print.benfords()
##        3 - Create save.benfords()


rm(list = ls())

# Task1 - Create benford.violations()
# We are going to create a function to test whether a group of number has violated the 
# Benford's law. There are two arguments: x, a vector or matrix, and type, "m", "d" or "b".
Benford.violations<-function (x, type = "b"){
  props<-table(factor(substr(as.character(x),1,1), levels = 1:9))/length(x)
  #To get started, we need to collect every first number, it must be a number in 1:9.
  if(type == "m"){
    m <- max(props-log(1+1/c(1:9),10))
    return(list(props, m=m))
    #The output is always a list, containing both the proportions and the index.
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
  #Based on the "type" the user chooses, the function calculate the corresponding 
  #statistics: m, d, or both.
}

# - test:
set.seed(268)
#We test with different numbers.
Benford.violations(sample(83974,735))
#Since the default type is "both", wen can get both "m" and "d" by leaving the second 
#argument blank.
Benford.violations(seq(390:9902), "m")
Benford.violations(rep(834,1298),"d")


# Task2 - Create print.benfords()
# We are going to create a function to print the above result in a users friendly way.
print.benfords <- function(x, type = "b"){
  imput <- Benford.violations(x=x, type=type)
  #Give the same arguments as Benford.violations(). Record the results in a new object 
  #called "imput". Now the statistics we get (m or d or both) are ready to use.
  if(type == "m"){
    M <- imput$m
    #Grabe the corresponding index we need from "imput".
    M <- ifelse(M < 0.851, M,
                ifelse(M < 0.967, M <- paste0(M, '*'),
                       ifelse(M < 1.212, M <- paste0(M, '**'),
                              M <- paste0(M, '***'))))
    #Depending on the specifc numbers, we print zero, one, two or three stars right 
    #next to the number.
  }
  if(type == "d"){
    D <- imput$d
    D <- ifelse(D < 1.212, D, 
                ifelse(D < 1.330, D <- paste0(D, '*'),
                       ifelse(D < 1.569, D <- paste0(D, '**'),
                              D <- paste0(D, '***'))))
    #It is similarly for d.
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
  # Depending on what the "type" is. We define differnt threshold to print the stars.
  
  # We are going to "visualize" the table with the following codes.  
  title <- "Benford's Law Test Statistics\n\n"
  leemis_label <- "Leemis' m\n"
  CG_D_label <- "Cho-Gains' d\n"
  dividing_line <- "_____________________________\n"
  legend <- "* signifies p<0.10\n ** signifies p<0.05\n *** signifies p<0.01"
  #Write down the title, label and legend. We would need part or all of them.
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
  #Still, depending on what the "type" is. We print out different content.
}  

# - test:
print.benfords(sample(23454,3435), type="b") 
print.benfords(seq(390:9902), type="m") 
print.benfords(rep(1,10000), type="d") 

# Task3 - Create save.benfords()
# Here, we only need to diverts the above output to a connection. 
# Besides "x" and "type", we incluse a third argument, file_name. That is to say the 
# user need to name the csv. file by himself. Remember: alway incluse ".csv" as part 
# of the third argument.
save.benfords <- function(x, type="b", file_name="Benfords_Law.csv"){
  sink(file_name)
  #First, create the file.
  print.benfords(x)
  sink()
  #Second, print the results in that file.
}

# - test:
save.benfords(sample(23454,3435), type="b")
save.benfords(seq(390:9902), "m", "Another test.csv")

library(tidyverse)
library(dplyr)
create_hex_code<-function(n=3){
  vec_digit<-c(letters[1:6],0:9)
  hex_code<-c()
  for(i in 1:n){
    hex_code<-c(hex_code,paste0("#",paste0(sample(vec_digit,6,replace=TRUE),collapse="")))
  }
return(hex_code)
}

create_hex_code()

library(readr)
library(ggplot2)
library(dplyr)

read_file_cvs <- function(NameFile,is_read_col_name){
  frequencies <- read_csv(NameFile, col_names = is_read_col_name)
  df <-data.frame(frequencies)
  return (df)
}

get_freq_from_df<- function(df){
  df_with_freq <- as.data.frame(table(unlist(df)))
  return (df_with_freq)
}

plot_df <- function(df_with_freq, x_lab,y_lab){
  ggplot(df_with_freq,mapping = aes(x= reorder(Var1,Freq), Freq)) +
    geom_bar(stat ="identity") + coord_flip()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
    geom_text(aes(label = signif(Freq, digits = 3)), nudge_y = 1) + 
    labs(title="",
         x=x_lab, y= y_lab) 
}


main <- function(){
  NameFile <- "Desktop/graphic_freq_in_r/Keywords/Keyword.csv"
  x_lab = "Keywords"
  y_lab = "Frequencies"
  is_read_col_name = F
  
  df <- read_file_cvs(NameFile = NameFile, is_read_col_name = is_read_col_name)
  print(length(df))
  df_with_freq <- get_freq_from_df(df)
  print(length(df_with_freq$Var1))
  plot_df(df_with_freq,x_lab,y_lab)
  

}

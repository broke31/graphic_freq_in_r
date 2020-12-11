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
  namefile = "OriginalKeywordsSnowballing"
  base = "Desktop/graphic_freq_in_r/Keywords/"
  path = paste(base,namefile, ".csv",sep = "")
  x_lab = "Keywords"
  y_lab = "Frequencies"
  is_read_col_name = F
  
  df <- read_file_cvs(NameFile = path, is_read_col_name = is_read_col_name)
  print(length(df))
  df_with_freq <- get_freq_from_df(df)
  print(length(df_with_freq$Var1))
  df_with_freq_order <-df_with_freq[order(-df_with_freq$Freq),]
  print(df_with_freq_order)
  path_out = paste(base, namefile,"_out",".csv",sep = "")
  write.csv(df_with_freq_order,path_out, row.names = F)
  plot_df(df_with_freq,x_lab,y_lab)
  

}

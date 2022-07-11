# Setting of directories of all the datas

  setwd("D:/Desktop D/Heart-Disease-App")
  Data <- read.csv("heart.csv",header=TRUE, sep=",",fill= TRUE)
  
  Data[, "sex"] <- as.factor(Data[,"sex"])
  Data[, "cp"] <- as.factor(Data[,"cp"])
  Data[, "fbs"] <- as.factor(Data[,"fbs"])
  Data[, "restecg"] <- as.factor(Data[,"restecg"])
  Data[, "exang"] <- as.factor(Data[,"exang"])
  Data[, "slope"] <- as.factor(Data[,"slope"])
  Data[, "ca"] <- as.factor(Data[,"ca"])
  Data[, "thal"] <- as.factor(Data[,"thal"])
  Data[, "target"] <- as.factor(Data[,"target"])

# Calculation of the decision tree

  library(tree)
  Tree <- tree(target ~ age + sex + cp + trestbps + chols + fbs
               + restecg + thalach + exang + oldpeak
               + slope + ca + thal, data=Data)
  
  # Tree pruning 
#  tuning <- cv.tree(Tree, K=10)
#  t <- which.min(tuning$dev)
#  Anzahl.Endknoten <- tuning$size[t]
#  model <- prune.tree(Tree,best=Anzahl.Endknoten)
  
# The model if we decided not to prune the Tree 
  model <- Tree
 
# Start shiny app

  library(shiny)
  library(bslib)
  # theme = bs_theme(
    # bg = "#101010", 
    # fg = "#FDF7F7", 
    # primary = "#FDF7F7", 
    # base_font = font_google("Luxurious Roman", local = TRUE),
    # code_font = font_google("Times New Roman", local = TRUE)
    # )
  runApp("App-Heart-Disease")
  
  
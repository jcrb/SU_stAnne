# fonctions pour Ste anne

#'@title sau
#'@description récupère les fichiers contenus dans un dossier et les assemble
#'le nom du fichier doit être au format AAAA_MM_hopNom.csv
#'La structure du fichier doit correspondre au format du serveur régional
#'@param an année au format 'AAAA'
#'@param hop nom de la structure tel qu'il apparait dans le nom du fichier
#'@param min mois de début (1-12)
#'@param max mois de fin (1-12)
#'@usage data<-sau("2013","stanne.csv")
#'@maj 2014-01-05
#'
sau<-function(an="2013",hopNom="stanne.csv",min=1,max=12){
  an<-"2013"
  hopNom<-"stanne.csv"
  for(i in min:max){
    if(nchar(i)<2){
      mois=paste("0",i,sep="")
    }
    else
      mois=as.character(i)
    file<-paste(an, mois, hopNom, sep="_")
    print(file)
    file <- paste("data", file, sep="/")
    f<-read.table(file,sep=",",header=TRUE,skip=2)
    if(i==1)
      data<-f
    else
      data<-rbind(data,f)
  }
  colnames(data)<-c("date","finess","service","inf1an","entre1_75ans","sup75ans","total","hospitalises","UHCD","tranferts")
  data$date<-as.Date(data$date,format="%d/%m/%Y")
  return(data)
}

# Amélioration:
# 
# The first line paste the path where R must look at for the files. 
# The second line creates an empty data object to store each of the importing files if any. 
# The third line reads the path to the files, and then a loop for reading each existing file of type ".txt" as table. The last line in the loop creates the final table by appending each subset that was imported into memory. 
# Finally, the last part of the program, which is out of the loop for efficiency purpose, simply write the final table to the disk as a text file, delimiting the columns by semicolon ';'.
# 
# path = "~/Documents/My Data/BRAZIL/Elections/"
# out.file<-""
# file.names <- dir(path, pattern =".txt")
# for(i in 1:length(file.names)){
#   file <- read.table(file.names[i],header=TRUE, sep=";", stringsAsFactors=FALSE)
#   out.file <- rbind(out.file, file)
# }
#  write.table(out.file, file = "cand_Brazil.txt",sep=";", 
#              row.names = FALSE, qmethod = "double",fileEncoding="windows-1252")
# 
# source: https://gist.github.com/danielmarcelino
#              


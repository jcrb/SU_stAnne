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
    file<-paste(an,mois,hopNom,sep="_")
    print(file)
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
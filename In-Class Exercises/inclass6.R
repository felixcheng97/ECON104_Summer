
library("stargazer")


GTD=read.csv("GTD.csv",stringsAsFactors=FALSE)
GTD=GTD[,c(3,5,11,12,13,17,20)]
levels(GTD$COUNTRY)
GTD=GTD[GTD$COUNTRY!="",]
GTD=GTD[GTD$INJURED!="Unknown",]
GTD=GTD[GTD$FATALITIES!="Unknown",]
GTD=GTD[GTD$INJURED!="",]
GTD=GTD[GTD$FATALITIES!="",]

GTD$COUNTRY=as.factor(GTD$COUNTRY)
GTD$TARGET.TYPE.1=as.factor(GTD$TARGET.TYPE.1)
GTD$ATTACK.TYPE.1=as.factor(GTD$ATTACK.TYPE.1)
GTD$WEAPON.TYPE.1=as.factor(GTD$WEAPON.TYPE.1)

levels(GTD$COUNTRY)
levels(GTD$PERPETRATOR.1)
levels(GTD$TARGET.TYPE.1)
levels(GTD$ATTACK.TYPE.1)
levels(GTD$WEAPON.TYPE.1)
summary(GTD)


#CONVERTING CASUALTIES
GTD$FATALITIES=as.numeric(GTD$FATALITIES)
GTD$INJURED=as.numeric(GTD$INJURED)
GTD$CASUALTIES=GTD$FATALITIES+GTD$INJURED
GTD$CASUALTIES=as.numeric(GTD$CASUALTIES>0)

#CONVERTING TARGET
GTD$TARGET=as.numeric(GTD$TARGET.TYPE.1=="Police")

#CONVERTING ATTACK
GTD$ATTACK=0

for(i in 1:length(GTD[,1])){

  if (GTD$ATTACK.TYPE.1[i]=="Hostage Taking (Barricade Incident)" | 
      GTD$ATTACK.TYPE.1[i]=="Hostage Taking (Kidnapping)" |
      GTD$ATTACK.TYPE.1[i]=="Hijacking" | 
      GTD$ATTACK.TYPE.1[i]=="Unarmed Assault" ){
    
    GTD$ATTACK[i]="CasualtyNotMainPurpose"     
    
  } else if (GTD$ATTACK.TYPE.1[i]=="Facility/Infrastructure Attack"){
    
    GTD$ATTACK[i]="Facility"     
    
  } else if (GTD$ATTACK.TYPE.1[i]=="Unknown"){
    
    GTD$ATTACK[i]="Unknown"    
    
  } else {
    
    GTD$ATTACK[i]="CasualtyIntended"  
    
  }
}

#CONVERTING WEAPON
GTD$WEAPON=0

for(i in 1:length(GTD[,1])){
  
  if (GTD$WEAPON.TYPE.1[i]=="Fake Weapons" | 
      GTD$WEAPON.TYPE.1[i]=="Sabotage Equipment" ){
    
    GTD$WEAPON[i]="NonCasualtyWeapon"     
    
  } else if (GTD$WEAPON.TYPE.1[i]=="Biological" | 
             GTD$WEAPON.TYPE.1[i]=="Chemical" |
             GTD$WEAPON.TYPE.1[i]=="Melee"){
    
    GTD$WEAPON[i]="NonFiringWeapon"    
    
  } else if (GTD$WEAPON.TYPE.1[i]=="Unknown" |
             GTD$WEAPON.TYPE.1[i]=="Other"){
    
    GTD$WEAPON[i]="Unknown"    
    
  } else {
    
    GTD$WEAPON[i]="FiringWeapon"  
    
  }
}

#WRAP-UP DATA
GTD=GTD[,c(1,8,9,10,11)]


#REGRESSIONS
regLM=lm(CASUALTIES~WEAPON+ATTACK+COUNTRY+TARGET,data=GTD)
regLogit=glm(CASUALTIES~WEAPON+ATTACK+COUNTRY+TARGET,
             family=binomial(link="logit") ,data=GTD)
regProbit=glm(CASUALTIES~WEAPON+ATTACK+COUNTRY+TARGET,
             family=binomial(link="probit") ,data=GTD)
stargazer(regLM,regProbit,regLogit,type="text")

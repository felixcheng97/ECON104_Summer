box=read.csv("box_office.csv")

box=box[,c(2,3,4,5,7,8)]

box$Release=as.Date(box$Release,format="%d-%b-%y")

box$month=format(as.Date(box$Release), "%m")


box$season=0

for (i in 1:(length(box[,1]))){
  
if (box$month[i] == "01" | box$month[i] == "02" |box$month[i] == 12){
  box$season[i]="winter"
} else if  (box$month[i] == "03" | box$month[i] == "04" |box$month[i] == "05"){
  box$season[i]="spring"
  
} else if(box$month[i] == "06" | box$month[i] == "07" |box$month[i] == "08"){
  box$season[i]="summer"
} else if(box$month[i] == "09" | box$month[i] == 10 |box$month[i] == 11){
  box$season[i]="fall"
}
}

box$season=as.factor(box$season)

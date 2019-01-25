
dat$d100_2_days = NA
head(dat)
dat$animalID=as.factor(dat$animalID)
dat$breed = as.factor(dat$breed)
dat$sex = as.factor(dat$sex)
dat$birthdate = as.Date(dat$birthdate)
dat$testdate = as.Date(dat$testdate)


if(!is.null(as.numeric(dat$testdate - dat$birthdate))){
  if(dat$sex == "F"){
    CF = (dat$weight/as.numeric(dat$testdate - dat$birthdate))*1.82604
    dat$d100_2_days = as.numeric(dat$testdate - dat$birthdate) - (dat$weight-100)/CF
  }else if(dat$sex == "M"){
    CF = (dat$weight/as.numeric(dat$testdate - dat$birthdate))*1.714615
    dat$d100_2_days = as.numeric(dat$testdate - dat$birthdate) - (dat$weight-100)/CF
  }else{
    dat$d100_2_days=NA
  }
}else{
  dat$d100_2_days=NA
}

BF = data.frame(Cul=rep(c("YY","LL","HP","DD"),each=2),
                Sex = rep(c("M","F"),4),
                A = c(12.402,13.706,12.826,13.983,13.113,14.288,13.468,15.654),
                B = c(0.10653,0.119624,0.114379,0.126014,0.11762,0.124425,0.111528,0.156646))
dat$d100_2_BF = NA
if(dat$breed == "YY"){
  if(dat$sex == "M"){
    Av = BF[BF$Cul=="YY"&BF$Sex == "M",]$A
    Bv = BF[BF$Cul=="YY"&BF$Sex == "M",4]
    CF = Av/(Av+(Bv*(dat$weight-100)))
    dat$d100_2_BF = dat$beakfat*CF
  }else if(dat$sex == "F"){
    Av = BF[BF$Cul=="YY"&BF$Sex == "F",3]
    Bv = BF[BF$Cul=="YY"&BF$Sex == "F",4]
    CF = Av/(Av+(Bv*(dat$weight-100)))
    dat$d100_2_BF = dat$beakfat*CF
  }else{
    dat$d100_2_BF = NA
  }
}else if(dat$breed == "LL"){
  if(dat$sex == "M"){
    Av = BF[BF$Cul=="LL"&BF$Sex == "M",]$A
    Bv = BF[BF$Cul=="LL"&BF$Sex == "M",4]
    CF = Av/(Av+(Bv*(dat$weight-100)))
    dat$d100_2_BF = dat$beakfat*CF
  }else if(dat$sex == "F"){
    Av = BF[BF$Cul=="LL"&BF$Sex == "F",3]
    Bv = BF[BF$Cul=="LL"&BF$Sex == "F",4]
    CF = Av/(Av+(Bv*(dat$weight-100)))
    dat$d100_2_BF = dat$beakfat*CF
  }else{
    dat$d100_2_BF = NA
  }
}else if(dat$breed == "HP"){
  if(dat$sex == "M"){
    Av = BF[BF$Cul=="HP"&BF$Sex == "M",]$A
    Bv = BF[BF$Cul=="HP"&BF$Sex == "M",4]
    CF = Av/(Av+(Bv*(dat$weight-100)))
    dat$d100_2_BF = dat$beakfat*CF
  }else if(dat$sex == "F"){
    Av = BF[BF$Cul=="HP"&BF$Sex == "F",3]
    Bv = BF[BF$Cul=="HP"&BF$Sex == "F",4]
    CF = Av/(Av+(Bv*(dat$weight-100)))
    dat$d100_2_BF = dat$beakfat*CF
  }else{
    dat$d100_2_BF = NA
  }
}else if(dat$breed == "DD"){
  if(dat$sex == "M"){
    Av = BF[BF$Cul=="DD"&BF$Sex == "M",]$A
    Bv = BF[BF$Cul=="DD"&BF$Sex == "M",4]
    CF = Av/(Av+(Bv*(dat$weight-100)))
    dat$d100_2_BF = dat$beakfat*CF
  }else if(dat$sex == "F"){
    Av = BF[BF$Cul=="DD"&BF$Sex == "F",3]
    Bv = BF[BF$Cul=="DD"&BF$Sex == "F",4]
    CF = Av/(Av+(Bv*(dat$weight-100)))
    dat$d100_2_BF = dat$beakfat*CF
  }else{
    dat$d100_2_BF = NA
  }
}


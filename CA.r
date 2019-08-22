#timestart <- Sys.time() #time test
####输入参数
t <- 400 #系统总计演化时间120mins = 7200s
L <- 133; #道路长度133,每个元胞7.5m
ltv <- 3; #车辆最大速度3 
Pslow <- 0.2 #随机慢化概率
l = 7.5 #元胞长度
duration = 13 #行人过马路时间

Pcararrive <- 0.5; #车辆到达概率
Pmancame <- 0.3; #行人出现的概率即红灯的概率
wait <- 10; #行人按下按钮后等待的时间

specimen <- c(ltv:1, rep(-1, ltv/Pcararrive-ltv)) #车辆出现的概率空间
crowd <- rbinom(t, size = 1, prob = Pmancame) - 1 #生成行人

crowevoluation <- function(crowd, t, wait, duration) {
  retained <- rep(0, t) # number of retained crowd
  crowdcount = crowd + 1
  s <- 3
  while( s < (t-wait+1)){
    if (crowd[s] == 0){
      retained[s:(s+wait)] <- cumsum(crowdcount[s:(s+wait)])
      #duration = ceiling(retained[s+wait]/4)+13-1 #行人过马路时间，排成四排
      #print(duration)
      crowd[s:(s+wait)] <- rep(-1, wait+1)
      crowd[(s+wait+1):(s+wait+duration)] <- rep(0, duration)
      s = s + wait + duration + 1
    }else {
      s = s + 1
    }
  }
  return(list(crowd = crowd, retained = retained))
}
walkman <- crowevoluation(crowd, t, wait, duration)
crossing = walkman$crowd
retain = walkman$retained #retained crowed
acce <- function(x, maxv, crossing, Pslow){
  n = length(x) #length of street
  ncars = sum(x<=maxv&x>-1) #number of cars
  where = which(x<=maxv&x>-1) #mark the sate of cars
  x[which(x<maxv&x>-1)] = x[which(x<maxv&x>-1)]+1 #acceleration
  x = c(x, crossing) #add the crossing
  gap = diff(which(x<=maxv&x>-1))-1 #the gap of cars
  if (crossing == -1){ # if the crossing is open
    gap = c(gap, maxv)
  }
  v = x[which(x<=maxv&x>-1)] #the car's speed
  v = v[-(ncars+1)]
  v[v>=(gap+1)] = gap[v>=(gap+1)] #deacceleration
  x[where] = v #change the street
  x = x[-(n+1)] #remove the crossing
  
  #moving the cars
  newx = rep(-1, n) #next second street
  sate = which(x>-1) #sate of cars
  carv = x[sate] #car's speeds
  nextsate = sate + carv #the next second sate of cars
  newx[nextsate] = carv
  newx = newx[1:n] #delete out cars
  
  #random slow down
  mark = which(newx>0)
  movingcars = newx[mark] #the car is moving
  nmove = length(movingcars)
  slow = rbinom(nmove, size = 1, prob = 1- Pslow) - 1 #the cars maybe deaccess
  movingcars = movingcars + slow
  newx[mark] = movingcars
  
  #satasict
  newv = newx[which(newx>-1)] 
  N = length(newv)
  muv = l*sum(newv)/N
  rho = N/(n*l)
  f = rho*muv
  stalled = sum(newx==0)
  
  results = list(newx = newx, N = N, muv = muv, rho = rho, f = f, stalled = stalled)
  #print(stalled)
  return(results)
}


sandtable <- data.frame(rep(-1, L))
V = c(); N = c(); Rho = c(); f = c(); Stalled = c()
for (i in 1:t) {
  newcome <- sample(specimen, size = 1)
  sandtable[1,i] <- newcome
  street = sandtable[,i]
  cross = crossing[i]
  results = acce(street, ltv, cross, Pslow)
  newstreet =results$newx
  
  sandtable = cbind(sandtable, newstreet)
  V = c(V, results$muv)
  N = c(N, results$N)
  Rho = c(Rho, results$rho)
  f = c(f, results$f)
  Stalled = c(Stalled, results$stalled)
}
#names(sandtable) = c(1:(t+1))
#image(t(as.matrix(sandtable)), col = grey.colors(255))

#par(mfrow=c(2,3))
#plot(Stalled, xlab = 'Time', ylab = "Stalled cars", type = "h")
#plot(V, xlab = "Time", ylab = "v(m/s)")
#plot(N, xlab = "Time", ylab = "N")
#plot(Rho, xlab = "Time", ylab = "rho")
#plot(f, xlab = "Time", ylab = "f")
#plot(retain, xlab = "Time", ylab = "retained crowed", type = "h")
#par(mfrow=c(1,1))

#timeend <- Sys.time()
#print(timeend-timestart)

setwd("/Users/jrw0107/Desktop/") #Google Drive/My Drive/teaching/WILD 7400 ABMs/

#Model question: How does transmission rate influence proportion of people infected?

#set up plot
plot(-100,-100,xlim=c(0,100), ylim=c(0,1), xlab="time", ylab="proportion infected")

#define variables
popsize = 1000
transmission = 0.02   #transmission rate
interact     = 50    #number of people each agent interacts with
colorsused   = "firebrick3" 

#initialize
people = data.frame(indv=seq(1,popsize,1), infected=rep(0,popsize))
people$infected[1:20] = 1
propinfected = NULL

#iterate model
for(r in 1:100){
  #record proportion of infected individuals and plot
  newcalc = nrow(people[people$infected==1,,drop=F])/nrow(people)
  points(x=r, y=newcalc, pch=19, col=colorsused)
  propinfected = c(propinfected, newcalc)
  
  #iterate over each person, randomly select people to interact with
  for(i in 1:popsize){
    #find a list of available people to associate with
    available = seq(1,nrow(people), 1)
    available = available[available!=i]
    
    #generate list of people to interact with from above
    targets = sample(available, interact)
    interacting = people[targets,]
    
    #isolate infected individuals
    interacting = interacting[interacting$infected==1,,drop=F]
    
    #determine if focal individual is infected
    if(nrow(interacting)>0){
      newlysick = sample(c(0,1), nrow(interacting), replace=T, prob=c((1-transmission), transmission))
      
      #set infection status
      people$infected[i] = max(newlysick)
    }
    
    }
  
}

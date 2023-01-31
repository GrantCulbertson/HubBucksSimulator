##Function to Estimate Hub Bucks Earned
hubBucks <- function(intervals  , trials){
  int_count = 0 
  chanceToCatch <- .95
  Misload <- 1/4620
  Salt <- .0001
  totalCount <- vector(length = intervals)
  while(int_count <= intervals){
    int_count = int_count + 1
    count = 0
    hold = vector(length=trials)
    caught = vector(length=trials)
    while(count <= trials){
      count2 = 1
      packagesForDay <- 1
      packagesForDay <- seq(packagesForDay , 120000 , 1)
      while(count2 <= 120000){
        count2 = count2 + 1
        if(rbinom(1 , 1 , Salt) == 1){
          packagesForDay[count2] = 0
        }
        if(rbinom(1 , 1 , Misload) == 1){
          packagesForDay[count2] = 0
          packagesForDay[count2]
        }
      }
      myFlow <- rpois(5 , 300)
      myFlow <- sum(myFlow)
      myPackages <- sample(packagesForDay  , myFlow, replace = FALSE)
      count = count + 1
      badToMe <- length(myPackages[myPackages == 0])
      hold[count] = badToMe
    }
    caught = 0
    for(package in hold){
      if(package == 0 & rbinom(1 , 1 , chanceToCatch) == 1){
        caught = caught + 1
      }
    }
    caught = caught %/% 3
    count2 = 0
    hubBucksCount = vector(length=caught)
    while(count2 < caught){
      count2 = count2 + 1
      if(rbinom(1 , 1 , .5) == 1){
        hubBucksCount[count2] = 5
      }else{
        hubBucksCount[count2] = 10
      }
    }
    print(int_count)
    totalCount[int_count] = sum(hubBucksCount)
  }
  return(totalCount)
}
buckResults <- hubBucks(1 , 12 * 5)

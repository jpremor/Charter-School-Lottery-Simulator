sc1OpenSpots = 60  #  Green Charter - estimated based on 900+ application in the website and 2-4 classes per grade, 20 students per class
sc1Applicants = 90

sc2OpenSpots = 150  # Greenville Tech
sc2Applicants = 575

sc3OpenSpots = 120  # Brashier - official info from the website
sc3Applicants = 340


simYearAppl = function(openSpots, applicantsQty) {
  
  #applicantsVect = data.frame(1:applicantsQty)
  lotteryVect = data.frame(sample(c(1:applicantsQty)))
  lotterySelec = lotteryVect[ c(1:openSpots) , ]
  applicNumb = round(runif(1,1,applicantsQty), digits=0)

  out = match(applicNumb, lotterySelec)
  if (is.na(out)) result = -1
  else result = 1
}


sevenYearsApplication = function() {
  lucky = 0
  year = 0
  
  # from 2021 to 2024, applying to Green Charter only
  
  # 2021 - 2nd grade
  if (lucky == 0) run = simYearAppl(sc1OpenSpots, sc1Applicants)
  if (run<0) {
    lucky = 0
  }
  if (run>0) { 
    lucky = 1 
    year = 2021
  }
  # 2022 - 3rd grade
  if (lucky == 0) {
    run = simYearAppl(sc1OpenSpots, sc1Applicants)
    if (run<0) {
      lucky = 0
    }
    if (run>0) { 
      lucky = 1 
      year = 2022
    }
    
    # 2023 = 4th grade
    if (lucky == 0) {
      run = simYearAppl(sc1OpenSpots, sc1Applicants)
      if (run<0) {
        lucky = 0
      }
      if (run>0) { 
        lucky = 1 
        year = 2023
      }
    
      # 2024 = 5th grade
      if (lucky == 0) {
        run = simYearAppl(sc1OpenSpots, sc1Applicants)
        if (run<0) {
          lucky = 0
        }
        if (run>0) { 
          lucky = 1 
          year = 2024
        }
        # 2025 = 6th grade
        if (lucky == 0) {
          run = simYearAppl(sc1OpenSpots, sc1Applicants)
          if (run<0) {
            lucky = 0
          }
          if (run>0) { 
            lucky = 1 
            year = 2025
          }
          # 2026 = 7th grade
          if (lucky == 0) {
            run = simYearAppl(sc1OpenSpots, sc1Applicants)
            if (run<0) {
              lucky = 0
            }
            if (run>0) { 
              lucky = 1 
              year = 2026
            }
            # 2027 = 8th grade
            if (lucky == 0) { 
              run = simYearAppl(sc1OpenSpots, sc1Applicants)
              if (run<0) {
                lucky = 0
              }
              if (run>0) { 
                lucky = 1 
                year = 2027 
              }
              # Teo applica pra elementary
              run = simYearAppl(sc1OpenSpots, sc1Applicants)
              if (run<0) {
                lucky = 0
              }
              if (run>0) { 
                lucky = 1 
                year = 2027 
              }
      
        
              # 2028 = 9th grade pra Alice e Teo Green, 2nd grade
              if (lucky == 0) {
                # 2028 = 9th grade Green - Teo
                run = simYearAppl(sc1OpenSpots, sc1Applicants)
                if (run<0) {
                  lucky = 0
                }
                if (run>0) { 
                  lucky = 1 
                  year = 2028
                }
                # 2028 = 9th grade Green - Alice
                run = simYearAppl(sc1OpenSpots, sc1Applicants) 
                if (run<0) {
                  lucky = 0
                }
                if (run>0) { 
                  lucky = 1 
                  year = 2028
                }
        
                # 2028 = 9th grade Greenville Tech
                run = simYearAppl(sc2OpenSpots, sc2Applicants)
                if (run>0) { 
                  lucky = lucky + 1 
                  year = 2028 
                  
                }
        
                # 2028 = 9th grade Brashier
                run = simYearAppl(sc3OpenSpots, sc1Applicants)
                if (run>0) { 
                  lucky = lucky + 1 
                  year = 2028
                }
              }
            }
          }
        }
      }
    }
  }
  
  if (lucky > 1) year = year+1000
  result = year
  
}


errorSim = c()
for (j in 1:10) {
  sim_school = c()
  count = 0
  count_highschool = 0
  for (i in 1:10000) {
    a = sevenYearsApplication()
    sim_school = c(sim_school, a)
    
    if (a>0) count = count + 1
    if (a>3000) count_highschool = count_highschool + 1
  }

  print( paste("Chances: ",(count)/100,"%", sep="") )
  print( paste("Chances High School: ",(count_highschool)/100,"%", sep="") )

  errorSim = c(errorSim, count/100)  
}


stddev = sd(errorSim)
print( paste("Average:", round(mean(errorSim),digits=2) ,"%    Error:",round(stddev*6,digits=2),"%", sep=""))












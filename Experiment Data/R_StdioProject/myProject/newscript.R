ITERATIONS = 5
SOURCE_FILE = "C:/Users/atifa/Desktop/ATLApproach+FitnessWiseComparison.txt"#inputfile
PAIRED_FILE = "C:/Users/atifa/Desktop/results.txt" #outputfile

general <-  function() 
{
  # alg   id found
  dt <- read.table(SOURCE_FILE,header=T)
  algs = sort(unique(dt$Algo))
  
  ids = sort(unique(dt$id))
  for(a in algs)
  {Pa
    
    f = c(1:length(ids))
    
    for(j in 1:length(ids))
    {
      id = ids[[j]]
      tmp =length(dt[dt$Algo==a & dt$id==id & dt$Found=='true',ITERATIONS])
      # print(tmp)
      f[[j]] = tmp
    }
    
    print(c(a,':',mean(f)))
  }
}


pairedTests <- function()
{
  # alg   id found
  
  dt <- read.table(SOURCE_FILE,header=T)
  
  algs = unique(dt$Algo)
  ids = sort(unique(dt$id))
  
  z = list() 
  
  for(i  in 1:length(algs))
  {	
    a = algs[[i]]
    
    f = c(1:length(ids))
    
    for(j in 1:length(ids))
    {
      id = ids[[j]]
      tot = length(dt[dt$Algo==a & dt$id==id ,ITERATIONS])
      suc = length(dt[dt$Algo==a & dt$id==id & dt$Found=='true',ITERATIONS])
      sr = suc/tot
      f[[j]] = sr 
    }
    
    z[[i]] = f 		
  }
  
  
  unlink(PAIRED_FILE)
  sink(PAIRED_FILE, append=TRUE, split=TRUE)
  
  cat(algs,"\n")
  cat(c("AVM","(1+1)EA","GA","RS"),"\n")
  
  cat("AVM:",z[[1]],"\n")
  cat("(1+1)EA",z[[2]],"\n")
  cat("GA",z[[3]],"\n")
  cat("RS",z[[4]],"\n")
  
  cat("\nPAIRED TESTS\n")
  
  for(x in 1:(length(algs)-1))
    for(y in (x+1):length(algs))
    {
      w = wilcox.test(z[[x]],z[[y]],paired=TRUE)
      cat("(",x,",",y,") pvalue = ",w$p.value,"\n")
    }
  
  sink()
}


successrateI <-  function()
{
  
  files <- c(1,1,2)
  files <- c("output_1.txt","output_2.txt")
  # alg   id found
  
  #dt <- read.table(paste("output_.txt", sep="",collapse=""),header=T)
  dt <- read.table(SOURCE_FILE,header=T)
  
  algos = sort(unique(dt$Algo))
  probs = sort(unique(dt$id))
  types = sort(unique(dt$Type))
  
  cat("Algo \t Id \t type \t TotRun \t SuccRun \t SR")
  cat("\n")
  
  for(a in algos)
  {	
    for(t in types)
    {
      for(prob in probs)
      {
        tot = length(dt[dt$Algo==a & dt$id==prob & dt$Type==t ,ITERATIONS])
        suc = length(dt[dt$Algo==a & dt$id==prob & dt$Type == t & dt$Found=='true',ITERATIONS])
        sr = suc/tot
        
        cat(a, "\t", prob, "\t", t, "\t", tot , "\t", suc, "\t", sr)
        cat("\n")
      }
    }
    
  }
  
}



successrate_pVal <-  function()
{
  
  files <- c(1,1,2)
  files <- c("output_1.txt","output_2.txt")
  # alg   id found
  
  #dt <- read.table(paste("output_.txt", sep="",collapse=""),header=T)
  dt <- read.table(SOURCE_FILE,header=T)
  
  algos = sort(unique(dt$Algo))
  probs = sort(unique(dt$id))
  types = sort(unique(dt$Type))
  
  cat("Algo \t Id \t type \t TotRun \t SuccRun \t SR")
  cat("\n")
  
  for(a in algos)
  {  
    for(t in types)
    {
      for(prob in probs)
      {
        
        tot = length(dt[dt$Algo==a & dt$id==prob & dt$Type==t ,ITERATIONS])
        suc = length(dt[dt$Algo==a & dt$id==prob & dt$Type == t & dt$Found=='true',ITERATIONS])
        sr = suc/tot
        iterationValues=dt[dt$Algo==a & dt$id==prob & dt$Type==t ,ITERATIONS];
        cat(a, "\t", prob, "\t", t, "\t", tot , "\t", suc, "\t", sr)
        cat("\n")
        
      }
      
    }
    
  }
}
successrate_table1 <-  function()
{
  files <- c(1,1,2)
  files <- c("output_1.txt","output_2.txt")
  # alg   id found
  
  #dt <- read.table(paste("output_.txt", sep="",collapse=""),header=T)
  dt <- read.table(SOURCE_FILE,header=T)
  
  algos = sort(unique(dt$Algo))
  probs = sort(unique(dt$id))
  types = sort(unique(dt$Type))
  
  
  cat("Algo \t Id \t S(IM)\t S(NIM) \t O/A \t p-value")
  cat("\n")
  
  for(a in algos)
  {  
    for(prob in probs)
    {
      pvA <- -1
      pvE <- -1
      eff<--99
      AVal<--999
      index <-1
      typeCount <-1
      tot = list()
      suc= list()
      fail= list()
      sr= list()
      
      for(t in types)
      {
        tot[index] = length(dt[dt$Algo==a & dt$id==prob & dt$Type==t ,ITERATIONS])
        suc[index] = length(dt[dt$Algo==a & dt$id==prob & dt$Type == t & dt$Found=='true',ITERATIONS])
        fail[index] = length(dt[dt$Algo==a & dt$id==prob & dt$Type == t & dt$Found=='false',ITERATIONS])
        sr[index] = suc[[index]]/tot[[index]]
        
        v=dt[dt$Algo==a & dt$id==prob & dt$Type=="Break" ,ITERATIONS];
        z=dt[dt$Algo==a & dt$id==prob & dt$Type=="Combined" ,ITERATIONS];
        
        
        index=index+1
        typeCount =typeCount +1
      }
      a1=suc[[1]]
      b=suc[[2]]
      n=tot[[1]]
      m=tot[[2]]
      c=fail[[1]]
      e=fail[[2]]
      sr_first = a1/n
      sr_other = b/m
      different_sr = FALSE
      
      if(!is.na(sr_first) & !is.na(sr_other) & sr_first != sr_other)
      {
        ### is it actually significant?
        
        d = 0.5
        
        #  	odds1 = ((a+d)/(n+d-a))  /  ((b+d)/(m+d-b)) 
        #		odds2 = ((b+d)/(m+d-b))  /  ((a+d)/(n+d-a)) 
        
        mat = matrix(c(a1,n-a1,b,m-b), 2 ,2)
        ft = fisher.test(mat,conf.int = FALSE)
        pvE = ft$p.value  
        
        #eff = ((a1+d)/(c+d)) * ((b+d)/(e+d))
        eff  = ((a1+d)*(e+d))   /  ((b+d)*(c+d))
        
        
        if(!is.nan(pvE) & pvE <= 0.05)
        {
          different_sr = TRUE
        }
      }
      ### no difference in success rate
      if(! different_sr)
      {
        ### now check steps, but only if both are finding something
        #  if(!is.na(sr_first) & !is.na(sr_other) & sr_first > 0 & sr_other >0)
        {
          w = wilcox.test(v,z,exact=FALSE)
          pvA = w$p.value
          
          if(is.nan(pvA)){
            pvA = 1
          }
          #if(!is.nan(pv) & pv <= 0.05)
          {
            #			cat("\nNo Difference in Success Rates - Comparing Iterations\n")
            
            AVal = measureA(v,z)
            
            #cat("Mean - Break ", mean(v)+1,"\n")
            #cat("Mean - Combined ", mean(z)+1,"\n")
            #print("A statistics")
            
            #cat("v:: ", v)
            #cat("\nz:: ", z)
            #cat("\n, A12 =", AVal, "\n")
            
          }
          
          
        }
        #  else{
        #     eff = -888
        
        # }
      }
      
      if(AVal != -999){ #we calculated the A12
        cat(a, "\t", prob, "\t", sr_first, "\t", sr_other , "\t", AVal, "\t", pvA, "\t i"  )
        
      }
      else{
        cat(a, "\t", prob, "\t", sr_first, "\t", sr_other , "\t", eff, "\t", pvE   )
      }
      
      cat("\n")
      
    }
    
  }
}  

measureA <- function(a,b)
{
  r = rank(c(a,b))
  r1 = sum(r[seq_along(a)])
  
  m = length(a)
  
  n = length(b)
  A = (r1/m - (m+1)/2)/n
  
  A
}  
successrate_table2 <-  function(){
  #files <- c(1,1,2)
  #files <- c("output_1.txt","output_2.txt")
  # alg   id found
  
  
  dt <- read.table(SOURCE_FILE,header=T)
  
  algos = sort(unique(dt$Algo))
  probs = sort(unique(dt$id))
  types = sort(unique(dt$Type))
  
  cat(" Comparison \t Id \t O/A \t p-value")
  cat("\n")
  
  # print(tmp)

  for(prob in probs)
  {  
    eff<- -99
    pvA <- -1
    pvE <- -1
    AVal <- -999
    index <- -1
    
    
    totAVM <- 0
    sucAVM <- 0
    failAVM <- 0
    srAVM <- 0.0
    totSSGA <- 0
    sucSSGA <- 0
    failSSGA <- 0
    srSSGA <- 0.0
    totOpOEA<- 0 
    sucOpOEA <- 0
    failOpOEA <- 0
    srOpOEA <- 0.0
    totRS <- 0
    sucRS <- 0
    failRS <- 0
    srRS <- 0.0
    
    vAVM=list()
    vSSGA=list()
    vOpOEA=list()
    vRS=list()
    
    
    for(a in algos)
    {
      
      if(a=="AVM")
      {
        totAVM = length(dt[dt$Algo==a & dt$id==prob & dt$Type=="Break" ,ITERATIONS])
        
        sucAVM = length(dt[dt$Algo==a & dt$id==prob & dt$Type == "Break" & dt$Found=='true',ITERATIONS])
        
        
        failAVM = length(dt[dt$Algo==a & dt$id==prob & dt$Type == "Break" & dt$Found=='false',ITERATIONS])
        srAVM = sucAVM/totAVM
        vAVM=dt[dt$Algo==a & dt$id==prob & dt$Type=="Break" ,ITERATIONS];	
      }
      if(a=="SSGA")
      {
        totSSGA = length(dt[dt$Algo==a & dt$id==prob & dt$Type=="Break" ,ITERATIONS])
        sucSSGA = length(dt[dt$Algo==a & dt$id==prob & dt$Type == "Break" & dt$Found=='true',ITERATIONS])
        failSSGA = length(dt[dt$Algo==a & dt$id==prob & dt$Type == "Break" & dt$Found=='false',ITERATIONS])
        srSSGA = sucSSGA/totSSGA
        vSSGA=dt[dt$Algo==a & dt$id==prob & dt$Type=="Break" ,ITERATIONS];	
        
      }
      if(a=="OpOEA")
      {
        totOpOEA = length(dt[dt$Algo==a & dt$id==prob & dt$Type=="Break" ,ITERATIONS])
        sucOpOEA = length(dt[dt$Algo==a & dt$id==prob & dt$Type == "Break" & dt$Found=='true',ITERATIONS])
        failOpOEA = length(dt[dt$Algo==a & dt$id==prob & dt$Type == "Break" & dt$Found=='false',ITERATIONS])
        srOpOEA = sucOpOEA/totOpOEA
        vOpOEA=dt[dt$Algo==a & dt$id==prob & dt$Type=="Break" ,ITERATIONS];	
        
      }
      if(a=="RS")
      {
        totRS = length(dt[dt$Algo==a & dt$id==prob & dt$Type=="Break" ,ITERATIONS])
        sucRS = length(dt[dt$Algo==a & dt$id==prob & dt$Type == "Break" & dt$Found=='true',ITERATIONS])
        failRS = length(dt[dt$Algo==a & dt$id==prob & dt$Type == "Break" & dt$Found=='false',ITERATIONS])
        srRS = sucRS/totRS
        vRS=dt[dt$Algo==a & dt$id==prob & dt$Type=="Break" ,ITERATIONS];
      }
    }
    
    
    #################### AVM vs EA
    
    
    a1=sucAVM
    b=sucOpOEA
    n=totAVM
    m=totOpOEA
    c=failAVM
    e=failOpOEA
    sr_first =srAVM  
    sr_other = srOpOEA 
    different_sr = FALSE
    v=vAVM
    z=vOpOEA
    
    cat("\n")
    cat("\nAVM v/s EA\t")
    calculateAndPrintValue(a1, b, v, z, sr_first, sr_other, prob, n, m, c, e)
    
    
    
    
    
    ############################# AVM vs RS
    eff<- -99
    pvA <- -1
    pvE <- -1
    AVal <- -999
    index <- -1
    
    
    a1=sucAVM
    b=sucRS
    n=totAVM
    m=totRS
    c=failAVM
    e=failRS
    sr_first =srAVM  
    sr_other = srRS 
    different_sr = FALSE
    v=vAVM
    z=vRS
    
    
    cat("\n")
    cat("\nAVM v/s RS\t")
    calculateAndPrintValue(a1, b, v, z, sr_first, sr_other, prob, n, m, c, e)
    
    
    
    
    
    ############################# AVM vs GA
    eff<- -99
    pvA <- -1
    pvE <- -1
    AVal <- -999
    index <- -1
    
    a1=sucAVM
    b=sucSSGA
    n=totAVM
    m=totSSGA
    c=failAVM
    e=failSSGA
    sr_first =srAVM  
    sr_other = srSSGA 
    different_sr = FALSE
    v=vAVM
    z=vSSGA
    
    cat("\n")
    cat("\nAVM v/s GA\t")
    
    calculateAndPrintValue(a1, b, v, z, sr_first, sr_other, prob, n, m, c, e)
    
    
    
    
    ############################# EA vs RS
    eff<- -99
    pvA <- -1
    pvE <- -1
    AVal <- -999
    index <- -1
    
    
    a1=sucOpOEA
    b=sucRS
    n=totOpOEA
    m=totRS
    c=failOpOEA
    e=failRS
    sr_first =srOpOEA  
    sr_other = srRS 
    different_sr = FALSE
    v=vOpOEA
    z=vRS
    
    cat("\n")
    cat("\nEA v/s RS\t")
    calculateAndPrintValue(a1, b, v, z, sr_first, sr_other, prob, n, m, c, e)
    
    
    
    ############################# EA vs GA
    eff<- -99
    pvA <- -1
    pvE <- -1
    AVal <- -999
    index <- -1
    
    a1=sucOpOEA
    b=sucSSGA
    n=totOpOEA
    m=totSSGA
    c=failOpOEA
    e=failSSGA
    sr_first =srOpOEA  
    sr_other = srSSGA 
    different_sr = FALSE
    v=vOpOEA
    z=vSSGA
    cat("\n")
    cat("\nEA v/s GA\t")
    
    calculateAndPrintValue(a1, b, v, z, sr_first, sr_other, prob, n, m, c, e)
    
    
    
    
    ############################# RS vs GA
    eff<- -99
    pvA <- -1
    pvE <- -1
    AVal <- -999
    index <- -1
    
    a1=sucRS
    b=sucSSGA
    n=totRS
    m=totSSGA
    c=failRS
    e=failSSGA
    sr_first =srRS  
    sr_other = srSSGA 
    different_sr = FALSE
    v=vRS
    z=vSSGA
    
    
    cat("\n")
    cat("\nRS v/s GA\t")
    calculateAndPrintValue(a1, b, v, z, sr_first, sr_other, prob, n, m, c, e)
    
    
  }
  
  
  
}







calculateAndPrintValue <-function(a1, b, v, z, sr_first, sr_other, prob, n, m, c, e)
{ AVal<--999
eff<-0.0
different_sr = FALSE
if(!is.na(sr_first) & !is.na(sr_other) & sr_first != sr_other)
{
  d = 0.5
  mat = matrix(c(a1,n-a1,b,m-b), 2 ,2)
  ft = fisher.test(mat,conf.int = FALSE)
  pvE = ft$p.value  
  
  
  eff  = ((a1+d)*(e+d))   /  ((b+d)*(c+d))
  
  
  if(!is.nan(pvE) & pvE <= 0.05)
  {
    different_sr = TRUE
    
  }
  
  
}

if(! different_sr)
{
  w = wilcox.test(v,z,exact=FALSE)
  pvA = w$p.value
  
  if(is.nan(pvA)){
    pvA = 1
  }
  AVal = measureA(v,z)
}




if(AVal != -999){ #we calculated the A12
  cat( prob, "\t", sr_first, "\t", sr_other , "\t", AVal, "\t", pvA, "\t i"  )
  
}
else{
  cat( prob, "\t", sr_first, "\t", sr_other , "\t", eff, "\t", pvE   )
}
cat("\n")

}
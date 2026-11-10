#' Fit Dirichlet distribution to diet composition data
#'
#' Performs bootstrap resampling and fits Dirichlet distributions to diet
#' composition data for each predator. Calculates diet statistics including
#' maximum likelihood estimates, confidence intervals, and probability density
#' functions for prey proportions.
#'
#' @param datafile Character. Name of CSV file containing diet composition data
#'   in wide format (columns: predator, wgt, prey1, prey2, ..., preyN).
#' @param pdafile Character. Name of output file for probability density
#'   array (mode values for each predator-prey combination).
#' @param resultfile Character. Name of output CSV file for diet statistics
#'   (lower95, upper95, mode, averages, etc.).
#' @param savepath Character. Directory path where output files will be saved.
#' @param thispredstage Character. Life stage of predators being analyzed
#'   (e.g., "juvenile", "adult", "invert").
#'
#' @return NULL. Results are written to CSV files specified by parameters.
#'   Output files include diet statistics and PDF distributions for each predator.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Bootstraps diet data (1000 iterations, 50 samples each)
#'   \item Fits Dirichlet distribution using VGAM package
#'   \item Derives marginal Beta distributions for each prey item
#'   \item Calculates maximum likelihood estimates (MLE) and 95% CI
#'   \item Computes probability density functions (PDF) across diet proportions
#'   \item Exports cumulative PDF for each predator-prey combination
#' }
#'
#' Diet proportions are squished slightly (0.00001 to 0.999) to avoid
#' numerical issues with Dirichlet fitting at 0 or 1.
#'
#' Requires at least 30 diet observations per predator for robust fitting.
#'
#' @importFrom data.table fread
#' @importFrom dplyr distinct pull select mutate filter group_by arrange
#' @importFrom VGAM vglm dirichlet Coef
#' @importFrom stats dbeta qbeta
#' @importFrom tibble as_tibble
#' @importFrom tidyr gather
#' @importFrom readr write_csv
#' @importFrom here here
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Run Dirichlet analysis for juvenile predators
#' do_dirichelet(
#'   datafile = "diet_composition_juvenile.csv",
#'   pdafile = "diet_pda_juvenile.csv",
#'   resultfile = "diet_dirichelet_juvenile.csv",
#'   savepath = "dirichelet/juvenile_dirichelet",
#'   thispredstage = "juvenile"
#' )
#'
#' # Run for adult predators
#' do_dirichelet(
#'   datafile = "diet_composition_adult.csv",
#'   pdafile = "diet_pda_adult.csv",
#'   resultfile = "diet_dirichelet_adult.csv",
#'   savepath = "dirichelet/adult_dirichelet",
#'   thispredstage = "adult"
#' )
#' }
do_dirichelet <- function(datafile, pdafile, resultfile, savepath, thispredstage){

  print(thispredstage)
  #read data
  x <- fread(here::here("outputs","dirichelet", datafile))

  npredspp  <- x %>%
    dplyr::distinct(predator) %>%
    dplyr::pull(predator) %>%
    length(.)

  nprey     <- ncol(x)-2

  predator.names <- x %>%
    dplyr::select(predator) %>%
    dplyr::distinct() %>%
    dplyr::pull(predator)

  options(digits=20)

  whatplot=2    # tells which plot to use
  #NOTE: We want to calculate all graphs at the same time, otherwise the bootstrap wont match
  #whatplot = 2 RedSnapperMarginalBetaA and B and also figs 1-4

  prey.names <- x %>% names() %>% .[-c(1:2)] %>% as.data.frame() %>% setNames("prey")

  #x <- x %>% as.data.frame()

  #dirichlet fit will fail if you feed it 0 or 1, so squish the values a bit on both sides
  dat2 <- cbind(x[,1:2],(0.999*(0.00001 + x[,3:(nprey+2)]/rowSums(x[,3:(nprey+2)]))))

  #set up matrices to hold results
  mat_names   <- list(c(colnames(dat2[,3:(nprey+2)])),c(levels(dat2$predator)))
  raw_wt_avg  <- matrix(nrow=nprey,ncol=npredspp,dimnames=mat_names)
  mle         <- matrix(nrow=nprey,ncol=npredspp,dimnames=mat_names)
  betavar     <- matrix(nrow=nprey,ncol=npredspp,dimnames=mat_names)
  lower95     <- matrix(nrow=nprey,ncol=npredspp,dimnames=mat_names)
  upper95     <- matrix(nrow=nprey,ncol=npredspp,dimnames=mat_names)
  mode_       <- matrix(nrow=nprey,ncol=npredspp,dimnames=mat_names)
  beta_a      <- matrix(nrow=nprey,ncol=npredspp,dimnames=mat_names)
  beta_b      <- matrix(nrow=nprey,ncol=npredspp,dimnames=mat_names)
  simpleaverage = matrix(nrow=npredspp,ncol=nprey)
  the_mode = matrix(nrow=nprey,ncol=npredspp)
  bootaverage = matrix(nrow=nprey,ncol=npredspp)
  bootmode = matrix(nrow=nprey,ncol=npredspp)


  if (whatplot==3){
    FirstPredToDo=1    #tells which predator to start at
    NumPredsToDo=length(predator.names)   #38 predator groups max, can be 1 0f 38 or use all 38 if want to loop through all predator groups
  }else{
    FirstPredToDo=1
    NumPredsToDo=length(predator.names)
  }

  #-------------------------------BOOTSTRAPPING-----------------------------------
  #for(i in 1:nlevels(dat2$Predator)){

  for(i in 1:length(predator.names)){
    print(i)
    this.predator <- predator.names[i]
    print(paste("This predator",this.predator))
    dat3 <- dat2 %>% dplyr::filter(predator==this.predator)

    if(nrow(dat3) >= 30) {

      simpleaverage[i,]=colMeans(dat3[,3:(nprey+2)])     #verified correct

      nstomachs   <- nrow(dat3)
      nfishavg    <- 50
      nbootstraps <- 1000
      stor        <- matrix(nrow=nbootstraps,ncol=nprey)
      tempstor <- matrix(nrow=nbootstraps,ncol=nprey)

      for(j in 1:nbootstraps){
        draw      <- dat3[sample(nstomachs,nfishavg,replace=T),]

        #-----
        tempstor[j,] <- apply(draw[,3:(nprey+2)],2,weighted.mean,w=draw$wgt)
        stor[j,] = tempstor[j,] / sum(tempstor[j,])

      }

      for (k in 1:nprey){
        bootaverage[k,i]=mean(stor[,k])
        bootmode[k,i]=as.numeric(names(sort(-table(stor[,k])))[1])
      }
      #---------------------------FIT TO DIRICHLET DIST-------------------------------
      #fit to distribution
      #stor[stor>1]=0.999999999
      #stor[stor<=0]=0.000000001

      print(i)
      print("trying to fit")
      fit <- VGAM::vglm(stor~1,dirichlet)
      print("fit worked")
      k   <- Coef(fit)

      #obtain marginal beta distribution parameters
      betaEst <- matrix(nrow=nprey,ncol=2)
      for(m in 1:nprey){
        betaEst[m,1] <- k[m]
        betaEst[m,2] <- sum(k)-k[m]
      }
      #find pdf of beta for each prey item looping over a range of diet proportions


      prop = exp(seq(log(0.0000000001), log(0.999), length.out = 10000))
      #prop=c(seq(0,0.009999, 0.000001),seq(0.01,0.0999,0.0001),seq(0.1,1,0.001))
      #prop=c(seq(0,0.09999,0.00001),seq(0.1,1,0.001))
      #prop=seq(0,1,0.0001)

      bdist   <- data.frame(matrix(nrow=length(prop),ncol=nprey))

      print("trying to populate bdist")

      for(n in 1:length(prop)){
        bdist[n,] <- dbeta(prop[n],betaEst[,1],betaEst[,2])
      }
      print("population successful")

      prey.names2 <- prey.names %>% .$prey %>% as.character()

      pdf_dist_sum <- bdist %>%
        as_tibble() %>%
        setNames(prey.names2) %>%
        mutate(predator=this.predator) %>%
        select(predator, everything()) %>%
        group_by(predator) %>%
        dplyr::mutate_each(dplyr::funs(sum))

      # PDF cumulative value
      pdf_dist_cumsum <- bdist %>%
        as_tibble() %>%
        setNames(prey.names2) %>%
        mutate(predator=this.predator) %>%
        select(predator, everything()) %>%
        group_by(predator) %>%
        dplyr::mutate_each(dplyr::funs(cumsum))

      pdf_dist_cont <- pdf_dist_cumsum

      for(eachcol in 2:ncol(pdf_dist_sum)){

        pdf_dist_cont[,eachcol] <- pdf_dist_cumsum[,eachcol] / pdf_dist_sum[,eachcol]
      }

      write.csv(pdf_dist_cont, paste(savepath,paste0(this.predator,"_PDF_",thispredstage, ".csv",sep=""), sep="/"))

      #-----------------------COMPUTE AND STORE THE STATISTICS----------------------------------
      #mean of raw data
      print("raw_wt_avg[,i] <- apply(dat3[,3:(nprey+2)],2,weighted.mean,w=dat3$wgt)")
      raw_wt_avg[,i] <- apply(dat3[,3:(nprey+2)],2,weighted.mean,w=dat3$wgt)
      #the_mode[,i] = apply(dat3[,3:(nprey+2)],2,mode)


      #maximum likelihood estimate
      print("Y <- apply(bdist,2,max)")
      Y <- apply(bdist,2,max)
      print("I <- apply(bdist,2,which.max)")
      I <- apply(bdist,2,which.max)

      print("maxes <- prop[c(I)]")
      maxes <- prop[c(I)]

      print("mle[,i] <- maxes")
      mle[,i] <- maxes
      #variance
      print("betavar[,i] <- betaEst[,1]*betaEst[,2]/((betaEst[,1]+betaEst[,2])^2 * (betaEst[,1]+betaEst[,2]+1))")
      betavar[,i] <- betaEst[,1]*betaEst[,2]/((betaEst[,1]+betaEst[,2])^2 * (betaEst[,1]+betaEst[,2]+1))
      #95% confidence intervals
      print("for(p in 1:nprey){")
      for(p in 1:nprey){
        lower95[p,i] <- qbeta(.025,betaEst[p,1],betaEst[p,2])
        upper95[p,i] <- qbeta(.975,betaEst[p,1],betaEst[p,2])

        #sensitivity of the MLE - change this range
        prop2=seq(0.000001,0.999999,0.000001)
        density_=-dbeta(prop2,betaEst[p,1],betaEst[p,2],log=TRUE)
        mode_[p,i] = prop2[which(density_ == min(density_))]

      }
      #Beta Parameters
      print("  beta_a[,i] <- t(betaEst)[1,]")
      beta_a[,i] <- t(betaEst)[1,]
      print("beta_b[,i] <- t(betaEst)[2,] ")
      beta_b[,i] <- t(betaEst)[2,]


    }


  } #iloopends

  npredlist  <- unique(x$predator)


  #write column names
  columnnames<-c("PredatorName","PreyName","lower95","upper95", "mode", "simpleaverages", "bootaverage", "bootmode")

  preynames=names(x)[3:length(names(x))]

  writeDF=columnnames

  for(i in FirstPredToDo:NumPredsToDo)  {

    mlesum = 0
    for(j in 1:nprey){
      mlesum = mlesum + mle[j,i]
    }

    for(j in 1:nprey){
      newrow = c(toString(npredlist[i]),preynames[j],lower95[j,i],upper95[j,i],(mle[j,i]/mlesum),raw_wt_avg[j,i],bootaverage[j,i],bootmode[j,i])
      writeDF=rbind(writeDF,newrow)

    }


  }

  write.csv(writeDF, file=here::here(savepath, resultfile), row.names=FALSE)

  write.csv(mode_,file=here::here(savepath, pdafile), row.names = FALSE)
  #---------------------------------OUTPUT----------------------------------------
  #output <- list(raw_wt_avg,mle,betavar,lower95,upper95,beta_a,beta_b)
  #names(output) <- c("raw_wt_avg","mle","betavar","lower95","upper95","beta_a","beta_b")
  #return(output)
  #mydata=read.csv("C:/Users/Michelle/Desktop/Atlantis/Diet/morethan10.csv")
  #FitDietDirichlet(mydata)
  #}
  #}


}


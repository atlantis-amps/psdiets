#' Read and process diet matrix from file
#'
#' Reads a diet composition CSV file and processes it by distributing
#' unidentified prey (FISH, INVERT, ROCKFISH, SALMON) to specific functional
#' groups, standardizing predator codes, and calculating final proportions.
#'
#' @param thisfile Integer. Index of file to read from diet.matrix.list vector.
#' @param diet.matrix.list Character vector. List of diet matrix filenames
#'   in the diet_data_files directory.
#'
#' @return Data frame with processed diet data containing columns:
#'   atlantis_pred_group, predator_stage, predator, atlantis_prey_group,
#'   prey, data, proportion, index.
#'
#' @details
#' This function requires several global objects that define group assignments:
#' \itemize{
#'   \item \code{groups.notinmodel}: Groups to exclude from analysis
#'   \item \code{invert.groups}, \code{all.fish.groups}: Valid prey groups
#'   \item \code{rockfish.groups}, \code{chinook.groups}, etc.: Species proportions
#'   \item \code{bird.groups}, \code{detritus.prey}: Aggregated group assignments
#' }
#'
#' Processing steps:
#' \enumerate{
#'   \item Reads CSV and standardizes column names
#'   \item Filters out non-diet items, zeros, and groups not in model
#'   \item Standardizes predator/prey codes (CI1/CI2, HE1, CO1)
#'   \item Distributes INVERT prey to specific invertebrate groups
#'   \item Distributes FISH prey to specific fish groups
#'   \item Distributes aggregate groups (ROCKFISH, CI, CO, CU, HE, BIRD, etc.)
#'   \item Recalculates indices and normalizes proportions
#' }
#'
#' Unidentified prey (FISH, INVERT) are distributed proportionally based on
#' identified prey in the same diet sample.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr filter select rename mutate left_join group_by summarise ungroup arrange distinct if_else
#' @importFrom here here
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Process first diet file
#' diet_data <- read_file_sheet(1, diet.matrix.list)
#'
#' # Process all diet files
#' diet.files <- 1:length(diet.matrix.list)
#' all_diets <- lapply(diet.files, read_file_sheet, diet.matrix.list)
#' combined_diets <- bind_rows(all_diets)
#' }
read_file_sheet <- function(thisfile, diet.matrix.list){

  print(thisfile)
  thisdatafile <- diet.matrix.list[thisfile]
  print(paste("Analyzing this file:",thisfile," ", thisdatafile))
  this.data <- read_csv(here::here("outputs","diet_data_files",thisdatafile))

  if("atlantis_predator_group" %in% names(this.data)){

    this.data <- this.data %>%
      dplyr::rename(atlantis_pred_group =atlantis_predator_group)
  }

  this.data.org <- this.data %>%
    dplyr::select(atlantis_pred_group,predator_stage,predator,atlantis_prey_group,prey,data,value,proportion,index,source) %>%
    filter(atlantis_prey_group!="NA") %>%  #eliminate prey that are not diet (rocks, wood)
    filter(value!=0) %>% #eliminate 0 values
    filter(!atlantis_prey_group %in% groups.notinmodel) %>%
    filter(!atlantis_pred_group %in% groups.notinmodel) %>%
    filter(!is.na(atlantis_pred_group)) %>%
    mutate(data = as.character(data)) %>%
    mutate(atlantis_prey_group = dplyr::if_else(atlantis_prey_group=="HE1","HEP",atlantis_prey_group)) %>%
    mutate(atlantis_pred_group = dplyr::if_else(atlantis_pred_group=="CI1","CHY",atlantis_pred_group)) %>%
    mutate(atlantis_pred_group = dplyr::if_else(atlantis_pred_group=="CI2","CHS",atlantis_pred_group)) %>%
    mutate(atlantis_pred_group = dplyr::if_else(atlantis_pred_group=="CO1","COY",atlantis_pred_group))
  #fish is allocated to identified fish groups

  prey.categories <- this.data.org %>%
    distinct(atlantis_prey_group) %>% .$atlantis_prey_group

  predator.categories <- this.data.org %>%
    distinct(atlantis_pred_group) %>% .$atlantis_pred_group

  if("INVERT" %in% prey.categories) {

    print("Data has inverts")
    # this only applies to data that have fish identified to species, if
    # only category is FISH it gets thrown out

    if(any(all.invert.groups %in% prey.categories)) {

      atlantis.fish.unid <- this.data.org %>%
        filter(atlantis_prey_group=="INVERT") %>% #select unidentified fish
        group_by(index) %>%
        summarise(sum_unid_fish=sum(value)) %>%
        dplyr::ungroup()

      index.unidentified <- atlantis.fish.unid %>% distinct(index) %>% .$index

      atlantis.fish.other <- this.data.org %>%
        filter(atlantis_prey_group!="INVERT") %>% #eliminate fish
        #select only fish groups
        filter(atlantis_prey_group %in% all.fish.groups)

      index.other <- atlantis.fish.other %>% distinct(index) %>% .$index

      if(any(index.unidentified %in% index.other)) {

        atlantis.fish.othersp <- this.data.org %>%
          filter(atlantis_prey_group!="INVERT") %>% #eliminate fish
          #select only fish groups
          filter(!atlantis_prey_group %in% all.fish.groups)

        atlantis.fish.other.indx <- atlantis.fish.other %>%
          filter(!index %in% index.unidentified)

        this.data.invert <- atlantis.fish.other %>%
          filter(index %in% index.unidentified) %>%
          mutate(index= as.character(index)) %>%
          group_by(index) %>%
          summarise(sum_tot_fish=sum(value), fish_groups = dplyr::n()) %>%
          dplyr::ungroup() %>%
          mutate(index= as.numeric(index)) %>%
          left_join(atlantis.fish.other, by="index") %>%
          mutate(fish_prop = value/ sum_tot_fish) %>%
          left_join(atlantis.fish.unid, by="index") %>%
          mutate(extra_fish = sum_unid_fish * fish_prop) %>%
          mutate(updated_value = extra_fish + value) %>%
          dplyr::select(-fish_prop,-sum_unid_fish,-value,-extra_fish, -sum_tot_fish,-fish_groups) %>%
          dplyr::rename(value=updated_value) %>%
          bind_rows(atlantis.fish.othersp, atlantis.fish.other.indx) %>%
          arrange(index,value)

      } else {


        this.data.invert <- this.data.org %>%
          filter(atlantis_prey_group!="INVERT")
      }

    } else {

      this.data.invert <- this.data.org %>%
        filter(atlantis_prey_group!="INVERT")

    }

  } else {

    this.data.invert <- this.data.org
  }

  if("FISH" %in% prey.categories) {
    print("Data has fish")
    # this only applies to data that have fish identified to species, if
    # only category is FISH it gets thrown out

    if(any(all.fish.groups %in% prey.categories)) {

      atlantis.fish.unid <- this.data.invert%>%
        dplyr::filter(atlantis_prey_group=="FISH") %>% #select unidentified fish
        dplyr::group_by(index) %>%
        dplyr::summarise(sum_unid_fish=sum(value)) %>%
        dplyr::ungroup()

      index.unidentified <- atlantis.fish.unid %>% distinct(index) %>% .$index

      atlantis.fish.other <- this.data.invert %>%
        dplyr::filter(atlantis_prey_group!="FISH") %>% #eliminate fish
        #select only fish groups
        dplyr::filter(atlantis_prey_group %in% all.fish.groups)

      index.other <- atlantis.fish.other %>% distinct(index) %>% .$index

      if(any(index.unidentified %in% index.other)) {

        atlantis.fish.othersp <- this.data.invert %>%
          dplyr::filter(atlantis_prey_group!="FISH") %>% #eliminate fish
          #select only fish groups
          dplyr::filter(!atlantis_prey_group %in% all.fish.groups)

        atlantis.fish.other.indx <- atlantis.fish.other %>%
          dplyr::filter(!index %in% index.unidentified)

        this.data.fish <- atlantis.fish.other %>%
          dplyr::filter(index %in% index.unidentified) %>%
          dplyr::mutate(index= as.character(index)) %>%
          dplyr::group_by(index) %>%
          dplyr::summarise(sum_tot_fish=sum(value), fish_groups = dplyr::n()) %>%
          dplyr::ungroup() %>%
          mutate(index= as.numeric(index)) %>%
          dplyr::left_join(atlantis.fish.other, by="index") %>%
          dplyr::mutate(fish_prop = value/ sum_tot_fish) %>%
          dplyr::left_join(atlantis.fish.unid, by="index") %>%
          dplyr::mutate(extra_fish = sum_unid_fish * fish_prop) %>%
          dplyr::mutate(updated_value = extra_fish + value) %>%
          dplyr::select(-fish_prop,-sum_unid_fish,-value,-extra_fish, -sum_tot_fish,-fish_groups) %>%
          dplyr::rename(value=updated_value) %>%
          dplyr::bind_rows(atlantis.fish.othersp, atlantis.fish.other.indx) %>%
          dplyr::arrange(index,value)

      } else {


        this.data.fish <- this.data.invert %>%
          dplyr::filter(atlantis_prey_group!="FISH")
      }

    } else {

      this.data.fish <- this.data.invert %>%
        dplyr::filter(atlantis_prey_group!="FISH")
    }

  } else {

    this.data.fish <- this.data.invert
  }


 rockfish.groups.pred <- rockfish.groups %>% dplyr::rename(atlantis_pred_group = atlantis_prey_group)

  if("ROCKFISH" %in% predator.categories) {

    print("Data has rockfish")
    this.data.rock.pred <-  organize_fish.pred(thesegroups = "ROCKFISH", thisdataset = rockfish.groups.pred, thisdata = this.data.fish)

  } else {

    this.data.rock.pred <- this.data.fish
  }

  if("ROCKFISH" %in% prey.categories) {

    this.data.rock <-  organize_fish(thesegroups = "ROCKFISH",thisdataset = rockfish.groups, thisdata = this.data.rock.pred)

  } else {

    this.data.rock <- this.data.rock.pred
  }


  if("CI" %in% prey.categories) {

    if("ROR" %in% predator.categories) {

      print("Data has resident orcas")
      this.data.orc <- this.data.rock %>%
        dplyr::filter(atlantis_pred_group=="ROR")

      this.data.other <-  this.data.rock %>%
        dplyr::filter(atlantis_pred_group!="ROR")

      this.data.ci <-  organize_fish(thesegroups = "CI", thisdataset = orca.chinook, thisdata = this.data.orc) %>%
        dplyr::bind_rows(this.data.other)

    } else {

      this.data.ci <-  organize_fish(thesegroups = "CI", thisdataset = chinook.groups, thisdata = this.data.rock)
    }
  } else {

    this.data.ci <- this.data.rock
  }

 print("Analyzing salmon groups")
  chinook.groups.pred <- chinook.groups %>% dplyr::rename(atlantis_pred_group = atlantis_prey_group)

 print("CHinook")
  if("CI" %in% predator.categories) {

    this.data.ci.pred <-  organize_fish.pred(thesegroups = "CI", thisdataset = chinook.groups.pred, thisdata = this.data.ci)

  } else {

    this.data.ci.pred <- this.data.ci
  }

  #CH is hatchery chinook
  if("CH" %in% prey.categories) {

    this.data.ch <-  organize_fish(thesegroups = "CH", thisdataset = chinook.hatchery.groups, thisdata = this.data.ci.pred)

  }  else {

    this.data.ch <- this.data.ci.pred
  }

  chinook.hatchery.groups.pred <- chinook.hatchery.groups %>% dplyr::rename(atlantis_pred_group = atlantis_prey_group)

  #CH is hatchery chinook
  if("CH" %in% predator.categories) {

    this.data.ch.pred <-  organize_fish.pred(thesegroups = "CH", thisdataset = chinook.hatchery.groups.pred, thisdata = this.data.ch)

  }  else {

    this.data.ch.pred <- this.data.ch
  }

  chinook.hatchery.groups.pred2 <- chinook.hatchery.groups.pred  %>% dplyr::mutate(atlantis_pred_group = "CI1_CI2")

  if("CI1_CI2" %in% predator.categories) {

    this.data.ch.pred2 <-  organize_fish.pred(thesegroups = "CI1_CI2", thisdataset = chinook.hatchery.groups.pred2, thisdata = this.data.ch.pred)

  }  else {

    this.data.ch.pred2 <- this.data.ch.pred
  }
  #coho
  print("Coho")
  if("CO" %in% prey.categories) {

    this.data.co <-  organize_fish(thesegroups = "CO", thisdataset = coho.groups, thisdata = this.data.ch.pred2)

  } else {

    this.data.co <- this.data.ch.pred2
  }

  coho.groups.pred <- coho.groups %>% dplyr::rename(atlantis_pred_group = atlantis_prey_group)

  #coho
  if("CO" %in% predator.categories) {

    this.data.co.pred <-  organize_fish.pred(thesegroups = "CO", thisdataset = coho.groups.pred, thisdata = this.data.co)

  } else {

    this.data.co.pred <- this.data.co
  }


  #Chum
  print("Chum")
  if("CU" %in% prey.categories) {

    this.data.cu <-  organize_fish(thesegroups = "CU", thisdataset = chum.groups, thisdata = this.data.co.pred)


  } else {

    this.data.cu <- this.data.co.pred
  }

  chum.groups.pred <- chum.groups %>% dplyr::rename(atlantis_pred_group = atlantis_prey_group)

  #Chum
  if("CU" %in% predator.categories) {

    this.data.cu.pred <-  organize_fish.pred(thesegroups = "CU", thisdataset = chum.groups.pred, thisdata = this.data.cu)


  } else {

    this.data.cu.pred <- this.data.cu
  }

  #Birds
  if("BIRD" %in% prey.categories) {

    this.data.bird.prey <-  organize_fish(thesegroups = "BIRD", thisdataset = bird.groups.prey, thisdata = this.data.cu.pred)

  } else {

    this.data.bird.prey <- this.data.cu.pred
  }

  if("BIRD" %in% predator.categories) {

    print("Data has birds")
    this.data.bird.pred <-  organize_fish.pred(thesegroups = "BIRD", thisdataset = bird.groups.pred, thisdata = this.data.bird.prey)

  } else {

    this.data.bird.pred <- this.data.bird.prey
  }

  #Detritus
  if("DETRITUS" %in% prey.categories) {

    this.data.det <-  organize_fish(thesegroups = "DETRITUS", thisdataset = detritus.prey, thisdata = this.data.bird.pred)

  } else {

    this.data.det <- this.data.bird.pred
  }

  #Herring
  if("HE" %in% prey.categories) {

    print("Data has herring")
    this.data.he <-  organize_fish(thesegroups = "HE", thisdataset = herring.groups, thisdata = this.data.det)

  } else {

    this.data.he <- this.data.det
  }

  herring.groups.pred <- herring.groups %>% dplyr::rename(atlantis_pred_group = atlantis_prey_group)

  if("HE" %in% predator.categories) {

    this.data.he.pred <-  organize_fish.pred(thesegroups = "HE", thisdataset = herring.groups.pred, thisdata = this.data.he)

  } else {

    this.data.he.pred <- this.data.he
  }


  if("WILD_SALMON" %in% prey.categories) {

    this.data.wisal <-  organize_fish(thesegroups = "WILD_SALMON", thisdataset = wild.salmon.groups, thisdata = this.data.he.pred)

  } else {

    this.data.wisal <- this.data.he.pred
  }

  if("HATCHERY_SALMON" %in% prey.categories) {

    this.data.hasal <-  organize_fish(thesegroups = "HATCHERY_SALMON", thisdataset = hatchery.salmon.groups, thisdata = this.data.wisal)

  } else {

    this.data.hasal <- this.data.wisal
  }

  if("BB" %in% prey.categories) {

    this.data.bac <-  organize_fish(thesegroups = "BB", thisdataset = bacteria.prey, thisdata = this.data.hasal)

  } else {

    this.data.bac <- this.data.hasal
  }

  if("CI_CO" %in% prey.categories) {

    this.data.cico <-  organize_fish(thesegroups = "CI_CO", thisdataset = chinook.coho.groups, thisdata = this.data.bac)

  } else {

    this.data.cico <- this.data.bac

  }


  this.data.org <-  this.data.cico %>%
    dplyr::group_by(source,data,atlantis_pred_group) %>%
    dplyr::mutate(index = dplyr::cur_group_id())

  this.data.fin <- this.data.org %>%
    dplyr::filter(!is.na(atlantis_prey_group)) %>%
    dplyr::filter(atlantis_prey_group!="NA") %>%
    dplyr::group_by(index) %>%
    dplyr::summarise(tot_value = sum(value)) %>%
    dplyr::left_join(this.data.org, by="index") %>%
    dplyr::mutate(proportion = value/tot_value) %>%
    dplyr::select(-tot_value, -value) %>%
    dplyr::arrange(index,atlantis_pred_group,atlantis_prey_group)

  print(ncol(this.data.fin))
  print(thisdatafile)
  print(this.data.fin %>% distinct(source))

  if(any(is.na(this.data.fin$atlantis_pred_group))==TRUE) stop ("value error")

  return(this.data.fin)
}

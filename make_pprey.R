eachpredcode <- func.groups %>%
  filter(`Long Name`==eachpredator) %>%
  pull(Code)


this.row <- pprey.diet.matrix %>%
  as.matrix() %>%
  toString() %>%
  gsub(",","",.)


if(predstage =="juvenile"){

  cat(paste(paste0("pPREY1",eachpredcode,"1"),"73"), file=prm.file.name, fill=TRUE, append=TRUE)
  cat(this.row, file=prm.file.name, fill=TRUE, append=TRUE)

  cat(paste(paste0("pPREY2",eachpredcode,"1"),"73"), file=prm.file.name, fill=TRUE, append=TRUE)
  cat(this.row, file=prm.file.name, fill=TRUE, append=TRUE)

}


if(predstage =="adult"){

  cat(paste(paste0("pPREY1",eachpredcode,"2"),"73"), file=prm.file.name, fill=TRUE, append=TRUE)
  cat(this.row, file=prm.file.name, fill=TRUE, append=TRUE)

  cat(paste(paste0("pPREY2",eachpredcode,"2"),"73"), file=prm.file.name, fill=TRUE, append=TRUE)
  cat(this.row, file=prm.file.name, fill=TRUE, append=TRUE)

}


if (predstage=="invert") {

  cat(paste(paste0("pPREY",eachpredcode),"73"), file=prm.file.name, fill=TRUE, append=TRUE)
  cat(this.row, file=prm.file.name, fill=TRUE, append=TRUE)

}





}


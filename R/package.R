if (!require(dplyr)){install.packages("dplyr");require(dplyr)}
all_na <- function(x) any(!is.na(x))
quote_string <- function(s) paste0('"', s, '"')
quote_string2 <- function(s) paste0('"', s, '",')
move_NAs_to_end <- function(row) {
  non_NAs <- row[!is.na(row)]
  NAs <- row[is.na(row)]
  c(non_NAs, NAs)
}

SPR_Setting <- function(
    NameOfPracticeItem = NULL,
    NameOfFillerItem = NULL,
    NameOfExperimentalItem = NULL,
    RandomOrderForQuestion = NULL) {
  
  Pname <- quote_string(NameOfPracticeItem)
  Fname <- quote_string(NameOfFillerItem)
  Ename <- quote_string(NameOfExperimentalItem)
  Qorder = if(RandomOrderForQuestion) "randomOrder: true}," else "randomOrder: false},"
  
  intro <- quote_string2("intro")
  setcounter <- quote_string2("setcounter")
  prepractice <- quote_string2("prepractice")
  separator <- quote_string2("sep")
  postpractice <- quote_string2("postpractice")
  sequence <- data.frame(
    "1" = paste0("var shuffleSequence = seq(", intro, " ", setcounter, " ", prepractice, " ", "sepWith(", separator, Pname, "), ", postpractice, 
                 " sepWith(", separator, " shuffle(randomize(", Fname, "), ", "rshuffle(startsWith(", Ename, ")))),", ' "', "sr", '", ', '"', "finish", '"', ");")
  )
  
  var_results <- data.frame(
    "1" = ("var manualSendResults = true;")
  )
  
  var_practice <- data.frame(
    "1" = paste0("var practiceItemTypes = [", Pname, "];")
  )

  var_defalutS <- data.frame("1" = "var defaults = [")
  
  DashedSentence <- data.frame("1" = ("\"DashedSentence\", {mode: \"self-paced reading\"},"))
  
  Question <- data.frame(
    "1" = paste0('"', "Question", '", ', "{hasCorrect: true,", Qorder)
  )

  Form <- data.frame("1" = ("\"Form\", {hideProgressBar: false, saveReactionTime: false},"))
  
  var_defaultE <- data.frame("1" = "];")
  
  var_itemS <- data.frame("1" = "var items = [")

  Introduction <- data.frame("1" = ("[\"intro\", \"Message\", {consentRequired: true, html: [\"div\",[\"p\", \"Welcome! In this experiment, you will read a series of sentences and answer questions about what you have read.\" ], [\"p\", \"Before you start reading, each sentence will be masked by a series of dashes as follows:\"], [\"p\", \"___ ___ ____ ______\"], [\"p\", \"To reveal the sentence, simply press the SPACE BAR. It will show the first word as follows:\"], [\"p\", \"The ___ ____ ______\"], [\"p\", \"Pressing the SPACE BAR again reveals the next word and masks the first word:\"], [\"p\", \"___ man ____ ______\"], [\"p\", \"At the end of each sentence, you will be presented with a yes/no comprehension question. To answer, press '1' if your response is 'yes' and '2' if it's 'no'.\"], [\"p\", \"To be able to read and answer quickly, keep your fingers on the SPACE BAR and the '1' and '2' keys to minimise distractions while reading and answering questions.\"], [\"p\", \"If you have any questions or need clarification, please contact the experimenter.\"], [\"p\", \"Before you begin, please click below to provide your consent to participate:\"], [\"p\", \"I agree to participate in this study and have read and understood the information above. I have been given the opportunity to ask questions about this study and these have been answered to my satisfaction. I understand that my data will be held in strict confidence, with no inclusion of personally identifiable information alongside the data, and that only anonymised data, specifically  data related to my responses, may be made accessible for potential future research purposes. I am aware that my involvement in this study is entirely voluntary, and I retain the right to withdraw from it at any time without the need for an explanation.\"]]}],"))
  
  prepractice <- data.frame("1" = ("[\"prepractice\", \"Message\", {html: \"The following items are for practice.\"}],"))
  
  postpractice <- data.frame("1" = ("[\"postpractice\", \"Message\", {html: \"End of practice. The experiment will begin next.\"}],"))
  
  sep <- data.frame("1" = ("[\"sep\", \"Separator\", {transfer: \"keypress\", normalMessage: \"Press any key to continue. Take a break at this point if needed.\", errorMessage: \"\", ignoreFailure: true}],"))
  
  SC <- data.frame(
    "1" = paste0("[", '"', "setcounter", '", ', '"', "__SetCounter__", '", ', "{ }],")
  )
  
  SR <- data.frame(
    "1" = paste0("[", '"', "sr", '", ', '"', "__SendResults__", '", ', "{ }],")
  )
  
  Finish <- data.frame(
    "1" = paste0("[", '"', "finish", '", ', '"', "Message", '", ', "{html: ", '"', "That is the end of the experiment. Thank you for your participation", '", ', "transfer: null}],")
  )
  
  dat <- dplyr::bind_rows(sequence, var_results, var_practice, var_defalutS, DashedSentence, Question, Form, var_defaultE, var_itemS, Introduction, prepractice, postpractice, SC, sep, SR, Finish)
  dat[is.na(dat)] <- " "
  dat
  }

Maze_Setting <- function(
    NameOfPracticeItem = NULL,
    NameOfFillerItem = NULL,
    NameOfExperimentalItem = NULL){
  
  Pname <- quote_string(NameOfPracticeItem)
  Fname <- quote_string(NameOfFillerItem)
  Ename <- quote_string(NameOfExperimentalItem)
  
  intro <- quote_string2("intro")
  setcounter <- quote_string2("setcounter")
  prepractice <- quote_string2("prepractice")
  separator <- quote_string2("sep")
  postpractice <- quote_string2("postpractice")
  sequence <- data.frame(
    "1" = paste0("var shuffleSequence = seq(", intro, " ", setcounter, " ", prepractice, " ", "followEachWith(", separator, Pname, "), ", postpractice, 
                 " followEachWith(", separator, " shuffle(randomize(", Fname, "), ", "rshuffle(startsWith(", Ename, ")))),", ' "', "sr", '", ', '"', "finish", '"', ");")
  )
  
  var_results <- data.frame(
    "1" = ("var manualSendResults = true;")
  )
  
  var_practice <- data.frame(
    "1" = paste0("var practiceItemTypes = [", Pname, "];")
  )
  
  var_defalutS <- data.frame("1" = "var defaults = [")
  
  Form <- data.frame("1" = ("\"Form\", {hideProgressBar: false, saveReactionTime: false},"))
  
  var_defaultE <- data.frame("1" = "];")
  
  var_itemS <- data.frame("1" = "var items = [")
  
  Introduction <- data.frame("1" = ("[\"intro\", \"Message\", {consentRequired: true, html: \"In this experiment, you'll be reading English sentences word by word while resting your fingers on the 'e', and 'i' keys. On each screen you will see two options: one will be the next word in the sentence, and one will not. Select the word that continues the sentence by pressing 'e' (left-hand) for the word on the left or pressing 'i' (right-hand) for the word on the right. <br><br> If you have any questions or need clarification, please contact the experimenter. <br><br> Before you begin, please click below to provide your consent to participate. <br><br> I agree to participate in this study and have read and understood the information above. I have been given the opportunity to ask questions about this study and these have been answered to my satisfaction. I understand that my data will be held in strict confidence, with no inclusion of personally identifiable information alongside the data, and that only anonymised data, specifically data related to my responses, may be made accessible for potential future research purposes. I am aware that my involvement in this study is entirely voluntary, and I retain the right to withdraw from it at any time without the need for an explanation.\"}],"))
  
  prepractice <- data.frame("1" = ("[\"prepractice\", \"Message\", {html: \"The following items are for practice.\"}],"))
  
  postpractice <- data.frame("1" = ("[\"postpractice\", \"Message\", {html: \"End of practice. The experiment will begin next.\"}],"))
  
  sep <- data.frame("1" = ("[\"sep\", \"MazeSeparator\", {normalMessage: \"Correct! Press any key to continue\", errorMessage: \"Incorrect! Press any key to continue.\"}],"))
  
  SC <- data.frame(
    "1" = paste0("[", '"', "setcounter", '", ', '"', "__SetCounter__", '", ', "{ }],")
  )
  
  SR <- data.frame(
    "1" = paste0("[", '"', "sr", '", ', '"', "__SendResults__", '", ', "{ }],")
  )
  
  Finish <- data.frame(
    "1" = paste0("[", '"', "finish", '", ', '"', "Message", '", ', "{html: ", '"', "That is the end of the experiment. Thank you for your participation", '", ', "transfer: null}],")
  )
  
  dat <- dplyr::bind_rows(sequence, var_results, var_practice, var_defalutS, Form, var_defaultE, var_itemS, Introduction, prepractice, postpractice, SC, sep, SR, Finish)
  dat[is.na(dat)] <- " "
  dat
}

Judgement_Setting <- function(
    NameOfPracticeItem = NULL,
    NameOfFillerItem = NULL,
    NameOfExperimentalItem = NULL,
    Scale = NULL,
    MaxComment = NULL,
    MinComment = NULL) {
  
  Pname <- quote_string(NameOfPracticeItem)
  Fname <- quote_string(NameOfFillerItem)
  Ename <- quote_string(NameOfExperimentalItem)
  
  intro <- quote_string2("intro")
  setcounter <- quote_string2("setcounter")
  prepractice <- quote_string2("prepractice")
  separator <- quote_string2("sep")
  postpractice <- quote_string2("postpractice")
  sequence <- data.frame(
    "1" = paste0("var shuffleSequence = seq(", intro, " ", setcounter, " ", prepractice, " ", "sepWith(", separator, Pname, "), ", postpractice, 
                 " sepWith(", separator, " shuffle(randomize(", Fname, "), ", "rshuffle(startsWith(", Ename, ")))),", ' "', "sr", '", ', '"', "finish", '"', ");")
  )
  
  var_results <- data.frame(
    "1" = ("var manualSendResults = true;")
  )
  
  var_practice <- data.frame(
    "1" = paste0("var practiceItemTypes = [", Pname, "];")
  )
  
  var_defalutS <- data.frame("1" = "var defaults = [")
  
  Form <- data.frame("1" = ("\"Form\", {hideProgressBar: false, saveReactionTime: false},"))
  
  vec_scale <- paste(paste0("\"", Scale, "\""), collapse = ", ")
  
  MaxCommentQ <- quote_string(MaxComment)
  MinCommentQ <- quote_string(MinComment)
  
  AcceptabilityJudgment <- data.frame("1" = paste0("\"AcceptabilityJudgment\", {as: ", "[", vec_scale, "],", 
  "presentAsScale: true, instructions: \"Use number keys or click boxes to answer.\",
  leftComment: ", MinCommentQ, ", rightComment: ",MaxCommentQ, "},"))
  
  var_defaultE <- data.frame("1" = "];")
  
  var_itemS <- data.frame("1" = "var items = [")
  
  Introduction <- data.frame("1" = paste0("[\"intro\", \"Message\", {consentRequired: true, html: \"Welcome! In this experiment, you'll be reading a series of sentences and assess their acceptability on a scale from ", min(Scale), " (", MinComment, ") ", "to ", max(Scale), " (", MaxComment, ")", "<br><br> If you have any questions or need clarification, please contact the experimenter. <br><br> Before you begin, please click below to provide your consent to participate. <br><br> I agree to participate in this study and have read and understood the information above. I have been given the opportunity to ask questions about this study and these have been answered to my satisfaction. I understand that my data will be held in strict confidence, with no inclusion of personally identifiable information alongside the data, and that only anonymised data, specifically data related to my responses, may be made accessible for potential future research purposes. I am aware that my involvement in this study is entirely voluntary, and I retain the right to withdraw from it at any time without the need for an explanation.\"}],"))
  
  prepractice <- data.frame("1" = ("[\"prepractice\", \"Message\", {html: \"The following items are for practice.\"}],"))
  
  postpractice <- data.frame("1" = ("[\"postpractice\", \"Message\", {html: \"End of practice. The experiment will begin next.\"}],"))
  
  sep <- data.frame("1" = ("[\"sep\", \"Separator\", {transfer: \"keypress\", normalMessage: \"Press any key to continue. Take a break at this point if needed.\", errorMessage: \"\", ignoreFailure: true}],"))
  
  SC <- data.frame(
    "1" = paste0("[", '"', "setcounter", '", ', '"', "__SetCounter__", '", ', "{ }],")
  )
  
  SR <- data.frame(
    "1" = paste0("[", '"', "sr", '", ', '"', "__SendResults__", '", ', "{ }],")
  )
  
  Finish <- data.frame(
    "1" = paste0("[", '"', "finish", '", ', '"', "Message", '", ', "{html: ", '"', "That is the end of the experiment. Thank you for your participation", '", ', "transfer: null}],")
  )
    
  dat <- dplyr::bind_rows(sequence, var_results, var_practice, var_defalutS, AcceptabilityJudgment, Form, var_defaultE, var_itemS, Introduction, prepractice, postpractice, SC, sep, SR, Finish)
  
  dat[is.na(dat)] <- " "
  dat
  
}

SPR <- function(
    Setting = NULL,
    Material = NULL,
    WordbyWordOrPhrasebyPhrase = NULL,
    NameOfPracticeItem = NULL,
    NumberOfCondition = NULL,
    NameOfExperimentalItem = NULL,
    NameOfFillerItem = NULL) {
  
  colnames(Material) <- paste0("c", 1:ncol(Material))
  
  pitem <- subset(Material, c1 == "p:")[-1]
  
  pitem <- pitem[, colSums(is.na(pitem)) != nrow(pitem)]
  
  br1 <- rep(c("["), nrow(pitem))
  nf <- paste0(br1, '"', rep(NameOfPracticeItem, nrow(pitem)), '",')
  task <- rep(paste0('"', "DashedSentence", '",'), nrow(pitem))
  s <- rep(c("{s:"), each = nrow(pitem))
  
  for (i in 1:nrow(pitem)) {
    if (WordbyWordOrPhrasebyPhrase == TRUE) {
      list <- matrix(NA, nrow = nrow(pitem), ncol = ncol(pitem))
      list <- apply(pitem, 2, function(d1) {
        d1 <- gsub("_", " ", d1)
        paste0('"', d1, '",')
      })
      
      material2 <- ifelse(list == '"",' | list == '""', "", list)
      material2 <- gsub('([:.:])([:":])([:,:])', '."', material2)
      material2[,1] <- paste0(br1, material2[,1])
      
      material2 <- data.frame(material2)
      
      for (i in 1:nrow(material2)) {
        last_col <- max(which(material2[i, ] != ""))
        material2[i, last_col] <- paste0(material2[i, last_col], "]}")
      }
    } else {
      
      pitem$c2[i] <- paste0('["', pitem$c2[i])
      last_col <- max(which(pitem[i, ] != ""))
      pitem[i, last_col] <- paste0(pitem[i, last_col], '"]}')
      
      material2 <- pitem
    }
  }
  
  cq <- paste0(', "', rep("Question", nrow(pitem)), '",') 
  br3 <- rep(c("{q:"), nrow(pitem))
  
  question <- subset(Material, c1 == "pq:")[-1]
  
  for (i in 1:nrow(question)) {
    if (is.na(question$c2[i])) {
      question$c2[i] <- "],"
    } else {
      question$c2[i] <- paste0('"', question$c2[i])
      last_col <- max(which(question[i, ] != ""))
      question[i, last_col] <- paste0(question[i, last_col], '",')
      
      question$c2[i] <- paste0(', "', "Question", '", ', "{q: ", question$c2[i])
    }
  }
  
  qoption <- subset(Material, c1 == "po:")[-1]
  
  qoption <- data.frame(lapply(qoption, function(x) {
    x <- trimws(x)
    x[!is.na(x) & x != "" & x != ","] <- paste0('"', x[!is.na(x) & x != "" & x != ","], '"')
    return(x)
  }))
  
  qoption <- data.frame(apply(qoption, 2, function(col) gsub("_", " ", col)))
  
  for (i in 1:nrow(qoption)) {
    if (is.na(qoption[i, 1])) {
      next
    } else {
      qoption[i, 1] <- paste0("as: [", qoption[i, 1])
      last_col <- max(which(qoption[i, ] != ""))
      qoption[i, last_col] <- paste0(qoption[i, last_col], "]} ],")
    }
  }
  
  pdat <- data.frame(nf, task, s, material2, question, qoption)
  pdat <- pdat %>% mutate_if(is.character, na_if, "")
  pdat <- pdat %>% select_if(all_na)
  colnames(pdat) <- 1:ncol(pdat)
  
  move_NAs_to_end <- function(row) {
    non_NAs <- row[!is.na(row)]
    NAs <- row[is.na(row)]
    c(non_NAs, NAs)
  }
  
  pdat <- t(apply(pdat, 1, move_NAs_to_end))
  pdat <- as.data.frame(pdat)
  
  sitem <- subset(Material, c1 == "s:")[-1]
  
  sitem <- sitem[, colSums(is.na(sitem)) != nrow(sitem)]
  
  sset <- nrow(sitem) / NumberOfCondition
  cond <- paste0("[[", '"', NameOfExperimentalItem, rep(1:NumberOfCondition, times = sset), '",')
  
  item <- paste0(rep(1:sset, each = NumberOfCondition), "],")
  
  task <- rep('"DashedSentence",', nrow(sitem))
  s <- rep("{s:", nrow(sitem))
  
  for (i in 1:nrow(sitem)) {
    if (WordbyWordOrPhrasebyPhrase == TRUE) {
      
      list <- apply(sitem, 2, function(d1) {
        paste0('"', gsub('_' ,' ',as.character(d1)), '",')
      })
      
      material2 <- ifelse(list == '"",' | list == '""', "", list)
      material2 <- gsub('([:.:])([:":])([:,:])', '."', material2)
      br2 <- rep("[", nrow(sitem))
      material2[,1] <- paste0(br2, material2[,1])
      
      for (i in 1:nrow(material2)) {
        last_col <- max(which(material2[i, ] != ""))
        material2[i, last_col] <- paste0(material2[i, last_col], "]}")
      }
      
      material2 <- data.frame(material2)
      
    } else {
      
      sitem$c2[i] <- paste0('["', sitem$c2[i])
      last_col <- max(which(sitem[i, ] != ""))
      sitem[i, last_col] <- paste0(sitem[i, last_col], '"]}')
      
      material2 <- sitem
    }
  }
  
  cq <- paste0(', "', rep("Question", nrow(sitem)), '",') 
  br4 <- rep(c("{q:"), nrow(sitem))
  
  question <- subset(Material, c1 == "sq:")[-1]
  
  for (i in 1:nrow(question)) {
    if (is.na(question$c2[i])) {
      question$c2[i] <- "],"
    } else {
      question$c2[i] <- paste0('"', question$c2[i])
      last_col <- max(which(question[i, ] != ""))
      question[i, last_col] <- paste0(question[i, last_col], '",')
      question$c2[i] <- paste0(', "', "Question", '", ', "{q: ", question$c2[i])
    }
  }
  
  qoption <- subset(Material, c1 == "so:")[-1]
  
  qoption <- data.frame(lapply(qoption, function(x) {
    x <- trimws(x)
    x[!is.na(x) & x != "" & x != ","] <- paste0('"', x[!is.na(x) & x != "" & x != ","], '"')
    return(x)
  }))
  
  qoption <- data.frame(apply(qoption, 2, function(col) gsub("_", " ", col)))
  
  for (i in 1:nrow(qoption)) {
    if (is.na(qoption[i, 1])) {
      next
    } else {
      qoption[i, 1] <- paste0("as: [", qoption[i, 1])
      last_col <- max(which(qoption[i, ] != ""))
      qoption[i, last_col] <- paste0(qoption[i, last_col], "], ")
    }
  }
  
  correct_response <- subset(Material, c1 == "sco:")[-1]
  
  for (i in 1:nrow(correct_response)) {
    if (is.na(correct_response$c2[i])) {
      next
    } else {
      correct_response[i, 1] <- paste0('"', correct_response[i, 1])
      last_col <- max(which(correct_response[i, ] != ""))
      correct_response[i, last_col] <- paste0(correct_response[i, last_col], '"}],')
    }
  }
  
  correct_response <- data.frame(apply(correct_response, 2, function(col) gsub("_", " ", col)))
  
  for (i in 1:nrow(qoption)) {
    if (is.na(qoption$c2[i])) {
      next
    } else {
      last_col <- max(which(qoption[i, ] != ""))
      qoption[i, last_col] <- paste0(qoption[i, last_col], "hasCorrect:")
    }
  }
  
  sdat <- data.frame(cond, item, task, s, material2, question, qoption, correct_response)
  sdat <- sdat %>% mutate_if(is.character, na_if, "")
  sdat <- sdat %>% select_if(all_na)
  colnames(sdat) <- 1:ncol(sdat)
  
  sdat <- t(apply(sdat, 1, move_NAs_to_end))
  sdat <- as.data.frame(sdat)
  
  fitem <- subset(Material, c1 == "f:")[-1]
  
  fitem <- fitem[, colSums(is.na(fitem)) != nrow(fitem)]
  
  nf <- paste0("[[", '"', rep(NameOfFillerItem, nrow(fitem)), '",')
  
  item <- paste0((sset + 1):(nrow(fitem) + sset), "],")
  
  task <- rep('"DashedSentence",', nrow(fitem))
  s <- rep("{s:", nrow(fitem))
  
  for (i in 1:nrow(fitem)) {
    if (WordbyWordOrPhrasebyPhrase == TRUE) {
      
      list <- apply(fitem, 2, function(d1) {
        paste0('"', gsub('_' ,' ',as.character(d1)), '",')
      })
      
      material2 <- ifelse(list == '"",' | list == '""', "", list)
      material2 <- gsub('([:.:])([:":])([:,:])', '."', material2)
      br2 <- rep("[", nrow(fitem))
      material2[,1] <- paste0(br2, material2[,1])
      
      for (i in 1:nrow(material2)) {
        last_col <- max(which(material2[i, ] != ""))
        material2[i, last_col] <- paste0(material2[i, last_col], "]}")
      }
      
      material2 <- data.frame(material2)
      
    } else {
      
      fitem$c2[i] <- paste0('["', fitem$c2[i])
      last_col <- max(which(fitem[i, ] != ""))
      fitem[i, last_col] <- paste0(fitem[i, last_col], '"]}')
      
      material2 <- fitem
    }
  }
  
  cq <- paste0('"',  rep("Question", nrow(fitem)), '",')
  br4 <- rep("{q:", nrow(fitem))
  
  question <- subset(Material, c1 == "fq:")[-1]
  
  for (i in 1:nrow(question)) {
    if (is.na(question$c2[i])) {
      question$c2[i] <- "],"
    } else {
      question$c2[i] <- paste0('"', question$c2[i])
      last_col <- max(which(question[i, ] != ""))
      question[i, last_col] <- paste0(question[i, last_col], '",')
      question$c2[i] <- paste0(', "', "Question", '", ', "{q: ", question$c2[i])
    }
  }
  
  qoption <- subset(Material, c1 == "fo:")[-1]
  
  qoption <- data.frame(lapply(qoption, function(x) {
    x <- trimws(x)
    x[!is.na(x) & x != "" & x != ","] <- paste0('"', x[!is.na(x) & x != "" & x != ","], '"')
    return(x)
  }))
  
  qoption <- data.frame(apply(qoption, 2, function(col) gsub("_", " ", col)))
  
  for (i in 1:nrow(qoption)) {
    if (is.na(qoption[i, 1])) {
      next
    } else {
      qoption[i, 1] <- paste0("as: [", qoption[i, 1])
      last_col <- max(which(qoption[i, ] != ""))
      qoption[i, last_col] <- paste0(qoption[i, last_col], "], ")
    }
  }
  
  correct_response <- subset(Material, c1 == "fco:")[-1] 
  
  for (i in 1:nrow(correct_response)) {
    if (is.na(correct_response$c2[i])) {
      next
    } else {
      correct_response[i, 1] <- paste0('"', correct_response[i, 1])
      last_col <- max(which(correct_response[i, ] != ""))
      correct_response[i, last_col] <- paste0(correct_response[i, last_col], '"}],')
    }
  }
  
  correct_response <- data.frame(apply(correct_response, 2, function(col) gsub("_", " ", col)))
  
  for (i in 1:nrow(qoption)) {
    if (is.na(qoption$c2[i])) {
      next
    } else {
      last_col <- max(which(qoption[i, ] != ""))
      qoption[i, last_col] <- paste0(qoption[i, last_col], "hasCorrect:")
    }
  }
  
  fdat <- data.frame(nf, item, task, s, material2, question, qoption, correct_response)
  fdat <- fdat %>% mutate_if(is.character, na_if, "")
  fdat <- fdat %>% select_if(all_na)
  colnames(fdat) <- 1:ncol(fdat)
  
  fdat <- t(apply(fdat, 1, move_NAs_to_end))
  fdat <- as.data.frame(fdat)
  
  var_itemE <- data.frame("1" = "];")
  dat <- dplyr::bind_rows(Setting, pdat, sdat, fdat, var_itemE)
  dat[is.na(dat)] <- " "
  
  write.table(dat, "SPR.js", quote = FALSE, row.names = FALSE, col.names = FALSE)
}

Maze <- function(
    Setting = NULL,
    Material = NULL,
    NameOfPracticeItem = NULL,
    NumberOfCondition = NULL,
    NameOfExperimentalItem = NULL,
    NameOfFillerItem = NULL) {
  
  quote_string <- function(s) paste0('"', s, '"')
  quote_string2 <- function(s) paste0('"', s, '", ')
  
  colnames(Material) <- paste0("c", 1:ncol(Material))
  
  pitem <- subset(Material, c1 == "p:")[-1]
  
  pitem <- pitem[, colSums(is.na(pitem)) != nrow(pitem)]
  
  nf <- quote_string2(NameOfPracticeItem)
  task <- quote_string2("Maze")
  
  for (i in 1:nrow(pitem)) {
    pitem$c2[i] <- paste0('"', pitem$c2[i])
    last_col <- max(which(pitem[i, ] != ""))
    pitem[i, last_col] <- paste0(pitem[i, last_col], '", ')
    pitem$c2[i] <- paste0("[", nf, task, " {s: ", pitem$c2[i])
  }
  
  pmitem <- subset(Material, c1 == "pm:")[-1]
  
  for (i in 1:nrow(pmitem)) {
    pmitem$c2[i] <- paste0('a: "', pmitem$c2[i])
    last_col <- max(which(pmitem[i, ] != ""))
    pmitem[i, last_col] <- paste0(pmitem[i, last_col], '"}],')
  }
  
  pdat <- data.frame(pitem, pmitem)
  
  pdat <- pdat %>% mutate_if(is.character, na_if, "")
  pdat <- pdat %>% select_if(all_na)
  colnames(pdat) <- 1:ncol(pdat)
  
  move_NAs_to_end <- function(row) {
    non_NAs <- row[!is.na(row)]
    NAs <- row[is.na(row)]
    c(non_NAs, NAs)
  }
  
  pdat <- t(apply(pdat, 1, move_NAs_to_end))
  pdat <- as.data.frame(pdat)
  
  sitem <- subset(Material, c1 == "s:")[-1]
  
  sitem <- sitem[, colSums(is.na(sitem)) != nrow(sitem)]
  
  sset <- nrow(sitem) / NumberOfCondition
  cond <- paste0("[[", '"', NameOfExperimentalItem, rep(1:NumberOfCondition, times = sset), '",')
  
  item <- paste0(rep(1:sset, each = NumberOfCondition), "],")
  
  task <- rep('"Maze",', nrow(sitem))
  s <- rep("{s:", nrow(sitem))
  
  for (i in 1:nrow(sitem)) {
    sitem$c2[i] <- paste0('"', sitem$c2[i])
    last_col <- max(which(sitem[i, ] != ""))
    sitem[i, last_col] <- paste0(sitem[i, last_col], '", ')
  }
  
  smitem <- subset(Material, c1 == "sm:")[-1]
  
  for (i in 1:nrow(smitem)) {
    smitem$c2[i] <- paste0('a: "', smitem$c2[i])
    last_col <- max(which(smitem[i, ] != ""))
    smitem[i, last_col] <- paste0(smitem[i, last_col], '"}],')
  }
  
  sdat <- data.frame(cond, item, task, s, sitem, smitem)
  
  sdat <- sdat %>% mutate_if(is.character, na_if, "")
  sdat <- sdat %>% select_if(all_na)
  colnames(sdat) <- 1:ncol(sdat)
  
  sdat <- t(apply(sdat, 1, move_NAs_to_end))
  sdat <- as.data.frame(sdat)
  
  fitem <- subset(Material, c1 == "f:")[-1]
  
  fitem <- fitem[, colSums(is.na(fitem)) != nrow(fitem)]
  
  nf <- paste0("[[", '"', rep(NameOfFillerItem, nrow(fitem)), '",')
  
  item <- paste0((sset + 1):(nrow(fitem) + sset), "],")
  
  task <- rep('"Maze",', nrow(fitem))
  s <- rep("{s:", nrow(fitem))
  
  for (i in 1:nrow(fitem)) {
    fitem$c2[i] <- paste0('"', fitem$c2[i])
    last_col <- max(which(fitem[i, ] != ""))
    fitem[i, last_col] <- paste0(fitem[i, last_col], '", ')
  }
  
  fmitem <- subset(Material, c1 == "fm:")[-1]
  
  for (i in 1:nrow(fmitem)) {
    fmitem$c2[i] <- paste0('a: "', fmitem$c2[i])
    last_col <- max(which(fmitem[i, ] != ""))
    fmitem[i, last_col] <- paste0(fmitem[i, last_col], '"}],')
  }
  
  fdat <- data.frame(nf, item, task, s, fitem, fmitem)
  
  fdat <- fdat %>% mutate_if(is.character, na_if, "")
  fdat <- fdat %>% select_if(all_na)
  colnames(fdat) <- 1:ncol(fdat)
  
  fdat <- t(apply(fdat, 1, move_NAs_to_end))
  fdat <- as.data.frame(fdat)
  
  var_itemE <- data.frame("1" = "];")
  dat <- dplyr::bind_rows(Setting, pdat, sdat, fdat, var_itemE)
  dat[is.na(dat)] <- " "
  
  write.table(dat, "Maze.js", quote = FALSE, row.names = FALSE, col.names = FALSE)
  
}

Judgement <- function(
    Setting = NULL,
    Material = NULL,
    NameOfPracticeItem = NULL,
    NumberOfCondition = NULL,
    NameOfExperimentalItem = NULL,
    NameOfFillerItem = NULL) {
  
  colnames(Material) <- paste0("c", 1:ncol(Material))
  
  pitem <- subset(Material, c1 == "p:")[-1]
  
  pitem <- pitem[, colSums(is.na(pitem)) != nrow(pitem)]
  
  nf <- quote_string2(NameOfPracticeItem)
  task <- quote_string2("AcceptabilityJudgment")
  
  for (i in 1:nrow(pitem)) {
    pitem$c2[i] <- paste0('"', pitem$c2[i])
    last_col <- max(which(pitem[i, ] != ""))
    pitem[i, last_col] <- paste0(pitem[i, last_col], '"}], ')
    pitem$c2[i] <- paste0("[", nf, task, " {s: ", pitem$c2[i])
  }
  
  pdat <- pitem
  
  pdat <- pdat %>% mutate_if(is.character, na_if, "")
  pdat <- pdat %>% select_if(all_na)
  colnames(pdat) <- 1:ncol(pdat)
  
  move_NAs_to_end <- function(row) {
    non_NAs <- row[!is.na(row)]
    NAs <- row[is.na(row)]
    c(non_NAs, NAs)
  }
  
  pdat <- t(apply(pdat, 1, move_NAs_to_end))
  pdat <- as.data.frame(pdat)
  
  sitem <- subset(Material, c1 == "s:")[-1]
  
  sitem <- sitem[, colSums(is.na(sitem)) != nrow(sitem)]
  
  sset <- nrow(sitem) / NumberOfCondition
  cond <- paste0("[[", '"', NameOfExperimentalItem, rep(1:NumberOfCondition, times = sset), '",')
  
  item <- paste0(rep(1:sset, each = NumberOfCondition), "],")
  
  task <- rep('"AcceptabilityJudgment",', nrow(sitem))
  s <- rep("{s:", nrow(sitem))
  
  for (i in 1:nrow(sitem)) {
    sitem$c2[i] <- paste0('"', sitem$c2[i])
    last_col <- max(which(sitem[i, ] != ""))
    sitem[i, last_col] <- paste0(sitem[i, last_col], '"}],')
  }
  
  sdat <- data.frame(cond, item, task, s, sitem)
  
  sdat <- sdat %>% mutate_if(is.character, na_if, "")
  sdat <- sdat %>% select_if(all_na)
  colnames(sdat) <- 1:ncol(sdat)
  
  sdat <- t(apply(sdat, 1, move_NAs_to_end))
  sdat <- as.data.frame(sdat)
  
  fitem <- subset(Material, c1 == "f:")[-1]
  
  fitem <- fitem[, colSums(is.na(fitem)) != nrow(fitem)]
  
  nf <- paste0("[[", '"', rep(NameOfFillerItem, nrow(fitem)), '",')
  
  item <- paste0((sset + 1):(nrow(fitem) + sset), "],")
  
  task <- rep('"AcceptabilityJudgment",', nrow(fitem))
  s <- rep("{s:", nrow(fitem))
  
  for (i in 1:nrow(fitem)) {
    fitem$c2[i] <- paste0('"', fitem$c2[i])
    last_col <- max(which(fitem[i, ] != ""))
    fitem[i, last_col] <- paste0(fitem[i, last_col], '"}],')
  }
  
  fdat <- data.frame(nf, item, task, s, fitem)
  
  fdat <- fdat %>% mutate_if(is.character, na_if, "")
  fdat <- fdat %>% select_if(all_na)
  colnames(fdat) <- 1:ncol(fdat)
  
  fdat <- t(apply(fdat, 1, move_NAs_to_end))
  fdat <- as.data.frame(fdat)
  
  var_itemE <- data.frame("1" = "];")
  dat <- dplyr::bind_rows(Setting, pdat, sdat, fdat, var_itemE)
  dat[is.na(dat)] <- " "
  
  write.table(dat, "Judgement.js", quote = FALSE, row.names = FALSE, col.names = FALSE)
  
}

SPRExp <- function(
    Material = NULL,
    NumberOfCondition = NULL,
    NameOfExperimentalItem = NULL,
    WordbyWordOrPhrasebyPhrase = NULL,
    StartFrom = NULL) {
  
  colnames(Material) <- paste0("c", 1:ncol(Material))

  sitem <- subset(Material, c1 == "s:")[-1]
  
  sitem <- sitem[, colSums(is.na(sitem)) != nrow(sitem)]
  
  sset <- nrow(sitem) / NumberOfCondition
  cond <- paste0("[[", '"', NameOfExperimentalItem, rep(1:NumberOfCondition, times = sset), '",')
  
  item <- paste0(rep(StartFrom:(sset + StartFrom - 1), each = NumberOfCondition), "],")
  
  task <- rep('"DashedSentence",', nrow(sitem))
  s <- rep("{s:", nrow(sitem))
  
  for (i in 1:nrow(sitem)) {
    if (WordbyWordOrPhrasebyPhrase == TRUE) {
      
      list <- apply(sitem, 2, function(d1) {
        paste0('"', gsub('_' ,' ',as.character(d1)), '",')
      })
      
      material2 <- ifelse(list == '"",' | list == '""', "", list)
      material2 <- gsub('([:.:])([:":])([:,:])', '."', material2)
      br2 <- rep("[", nrow(sitem))
      material2[,1] <- paste0(br2, material2[,1])
      
      for (i in 1:nrow(material2)) {
        last_col <- max(which(material2[i, ] != ""))
        material2[i, last_col] <- paste0(material2[i, last_col], "]}")
      }
      
      material2 <- data.frame(material2)
      
    } else {
      
      sitem$c2[i] <- paste0('["', sitem$c2[i])
      last_col <- max(which(sitem[i, ] != ""))
      sitem[i, last_col] <- paste0(sitem[i, last_col], '"]}')
      
      material2 <- sitem
    }
  }

  cq <- paste0(', "', rep("Question", nrow(sitem)), '",') 
  br4 <- rep(c("{q:"), nrow(sitem))
  
  question <- subset(Material, c1 == "sq:")[-1]
  
  for (i in 1:nrow(question)) {
    if (is.na(question$c2[i])) {
      question$c2[i] <- "],"
    } else {
      question$c2[i] <- paste0('"', question$c2[i])
      last_col <- max(which(question[i, ] != ""))
      question[i, last_col] <- paste0(question[i, last_col], '",')
      question$c2[i] <- paste0(', "', "Question", '", ', "{q: ", question$c2[i])
    }
  }
  
  qoption <- subset(Material, c1 == "so:")[-1]
  
  qoption <- data.frame(lapply(qoption, function(x) {
    x <- trimws(x)
    x[!is.na(x) & x != "" & x != ","] <- paste0('"', x[!is.na(x) & x != "" & x != ","], '"')
    return(x)
  }))
  
  qoption <- data.frame(apply(qoption, 2, function(col) gsub("_", " ", col)))
  
  for (i in 1:nrow(qoption)) {
    if (is.na(qoption[i, 1])) {
      next
    } else {
      qoption[i, 1] <- paste0("as: [", qoption[i, 1])
      last_col <- max(which(qoption[i, ] != ""))
      qoption[i, last_col] <- paste0(qoption[i, last_col], "], ")
    }
  }
  
  correct_response <- subset(Material, c1 == "sco:")[-1]
  
  for (i in 1:nrow(correct_response)) {
    if (is.na(correct_response$c2[i])) {
      next
    } else {
      correct_response[i, 1] <- paste0('"', correct_response[i, 1])
      last_col <- max(which(correct_response[i, ] != ""))
      correct_response[i, last_col] <- paste0(correct_response[i, last_col], '"}],')
    }
  }
  
  correct_response <- data.frame(apply(correct_response, 2, function(col) gsub("_", " ", col)))
  
  for (i in 1:nrow(qoption)) {
    if (is.na(qoption$c2[i])) {
      next
    } else {
      last_col <- max(which(qoption[i, ] != ""))
      qoption[i, last_col] <- paste0(qoption[i, last_col], "hasCorrect:")
    }
  }
  
  sdat <- data.frame(cond, item, task, s, material2, question, qoption, correct_response)
  sdat <- sdat %>% mutate_if(is.character, na_if, "")
  sdat <- sdat %>% select_if(all_na)
  colnames(sdat) <- 1:ncol(sdat)
  
  sdat <- t(apply(sdat, 1, move_NAs_to_end))
  sdat <- as.data.frame(sdat)
  
  sdat[is.na(sdat)] <- " "
  
  write.table(sdat, "SPRExp.js", quote = FALSE, row.names = FALSE, col.names = FALSE)
}

SPRFiller <- function(
    Material = NULL,
    NameOfFillerItem = NULL,
    WordbyWordOrPhrasebyPhrase = NULL,
    StartFrom = NULL) {
  
  colnames(Material) <- paste0("c", 1:ncol(Material))

  fitem <- subset(Material, c1 == "f:")[-1]
  
  fitem <- fitem[, colSums(is.na(fitem)) != nrow(fitem)]
  
  nf <- paste0("[[", '"', rep(NameOfFillerItem, nrow(fitem)), '",')
  
  item <- paste0(rep(StartFrom:(nrow(fitem) + StartFrom - 1)), "],")
  
  task <- rep('"DashedSentence",', nrow(fitem))
  s <- rep("{s:", nrow(fitem))
  
  for (i in 1:nrow(fitem)) {
    if (WordbyWordOrPhrasebyPhrase == TRUE) {
      
      list <- apply(fitem, 2, function(d1) {
        paste0('"', gsub('_' ,' ',as.character(d1)), '",')
      })
      
      material2 <- ifelse(list == '"",' | list == '""', "", list)
      material2 <- gsub('([:.:])([:":])([:,:])', '."', material2)
      br2 <- rep("[", nrow(fitem))
      material2[,1] <- paste0(br2, material2[,1])
      
      for (i in 1:nrow(material2)) {
        last_col <- max(which(material2[i, ] != ""))
        material2[i, last_col] <- paste0(material2[i, last_col], "]}")
      }
      
      material2 <- data.frame(material2)
      
    } else {
      
      fitem$c2[i] <- paste0('["', fitem$c2[i])
      last_col <- max(which(fitem[i, ] != ""))
      fitem[i, last_col] <- paste0(fitem[i, last_col], '"]}')
      
      material2 <- fitem
    }
  }
  
  
  
  cq <- paste0('"',  rep("Question", nrow(fitem)), '",')
  br4 <- rep("{q:", nrow(fitem))
  
  question <- subset(Material, c1 == "fq:")[-1]
  
  for (i in 1:nrow(question)) {
    if (is.na(question$c2[i])) {
      question$c2[i] <- "],"
    } else {
      question$c2[i] <- paste0('"', question$c2[i])
      last_col <- max(which(question[i, ] != ""))
      question[i, last_col] <- paste0(question[i, last_col], '",')
      question$c2[i] <- paste0(', "', "Question", '", ', "{q: ", question$c2[i])
    }
  }
  
  qoption <- subset(Material, c1 == "fo:")[-1]
  
  qoption <- data.frame(lapply(qoption, function(x) {
    x <- trimws(x)
    x[!is.na(x) & x != "" & x != ","] <- paste0('"', x[!is.na(x) & x != "" & x != ","], '"')
    return(x)
  }))
  
  qoption <- data.frame(apply(qoption, 2, function(col) gsub("_", " ", col)))
  
  for (i in 1:nrow(qoption)) {
    if (is.na(qoption[i, 1])) {
      next
    } else {
      qoption[i, 1] <- paste0("as: [", qoption[i, 1])
      last_col <- max(which(qoption[i, ] != ""))
      qoption[i, last_col] <- paste0(qoption[i, last_col], "], ")
    }
  }
  
  correct_response <- subset(Material, c1 == "fco:")[-1] 
  
  for (i in 1:nrow(correct_response)) {
    if (is.na(correct_response$c2[i])) {
      next
    } else {
      correct_response[i, 1] <- paste0('"', correct_response[i, 1])
      last_col <- max(which(correct_response[i, ] != ""))
      correct_response[i, last_col] <- paste0(correct_response[i, last_col], '"}],')
    }
  }
  
  correct_response <- data.frame(apply(correct_response, 2, function(col) gsub("_", " ", col)))
  
  for (i in 1:nrow(qoption)) {
    if (is.na(qoption$c2[i])) {
      next
    } else {
      last_col <- max(which(qoption[i, ] != ""))
      qoption[i, last_col] <- paste0(qoption[i, last_col], "hasCorrect:")
    }
  }
  
  fdat <- data.frame(nf, item, task, s, material2, question, qoption, correct_response)
  fdat <- fdat %>% mutate_if(is.character, na_if, "")
  fdat <- fdat %>% select_if(all_na)
  colnames(fdat) <- 1:ncol(fdat)
  
  fdat <- t(apply(fdat, 1, move_NAs_to_end))
  fdat <- as.data.frame(fdat)
  
  fdat[is.na(fdat)] <- " "
  
  write.table(fdat, "SPRFiller.js", quote = FALSE, row.names = FALSE, col.names = FALSE)
  }

MazeExp <- function(
    Material = NULL,
    NumberOfCondition = NULL,
    NameOfExperimentalItem = NULL,
    StartFrom = NULL) {
  
  quote_string <- function(s) paste0('"', s, '"')
  quote_string2 <- function(s) paste0('"', s, '", ')
  
  colnames(Material) <- paste0("c", 1:ncol(Material))
  
  sitem <- subset(Material, c1 == "s:")[-1]
  
  sitem <- sitem[, colSums(is.na(sitem)) != nrow(sitem)]
  
  sset <- nrow(sitem) / NumberOfCondition
  cond <- paste0("[[", '"', NameOfExperimentalItem, rep(1:NumberOfCondition, times = sset), '",')
  
  item <- paste0(rep(StartFrom:(sset + StartFrom - 1), each = NumberOfCondition), "],")
  
  task <- rep('"Maze",', nrow(sitem))
  s <- rep("{s:", nrow(sitem))
  
  for (i in 1:nrow(sitem)) {
    sitem$c2[i] <- paste0('"', sitem$c2[i])
    last_col <- max(which(sitem[i, ] != ""))
    sitem[i, last_col] <- paste0(sitem[i, last_col], '", ')
  }
  
  smitem <- subset(Material, c1 == "sm:")[-1]
  
  for (i in 1:nrow(smitem)) {
    smitem$c2[i] <- paste0('a: "', smitem$c2[i])
    last_col <- max(which(smitem[i, ] != ""))
    smitem[i, last_col] <- paste0(smitem[i, last_col], '"}],')
  }
  
  sdat <- data.frame(cond, item, task, s, sitem, smitem)
  
  sdat <- sdat %>% mutate_if(is.character, na_if, "")
  sdat <- sdat %>% select_if(all_na)
  colnames(sdat) <- 1:ncol(sdat)
  
  sdat <- t(apply(sdat, 1, move_NAs_to_end))
  sdat <- as.data.frame(sdat)
  
  sdat[is.na(sdat)] <- " "
  
  write.table(sdat, "MazeExp.js", quote = FALSE, row.names = FALSE, col.names = FALSE)
}

MazeFiller <- function(
    Material = NULL,
    NameOfFillerItem = NULL,
    StartFrom = NULL) {
  
  colnames(Material) <- paste0("c", 1:ncol(Material))
  
  fitem <- subset(Material, c1 == "f:")[-1]
  
  fitem <- fitem[, colSums(is.na(fitem)) != nrow(fitem)]
  
  nf <- paste0("[[", '"', rep(NameOfFillerItem, nrow(fitem)), '",')
  
  item <- paste0(rep(StartFrom:(nrow(fitem) + StartFrom - 1)), "],")
  
  task <- rep('"Maze",', nrow(fitem))
  s <- rep("{s:", nrow(fitem))
  
  for (i in 1:nrow(fitem)) {
    fitem$c2[i] <- paste0('"', fitem$c2[i])
    last_col <- max(which(fitem[i, ] != ""))
    fitem[i, last_col] <- paste0(fitem[i, last_col], '", ')
  }
  
  fmitem <- subset(Material, c1 == "fm:")[-1]
  
  for (i in 1:nrow(fmitem)) {
    fmitem$c2[i] <- paste0('a: "', fmitem$c2[i])
    last_col <- max(which(fmitem[i, ] != ""))
    fmitem[i, last_col] <- paste0(fmitem[i, last_col], '"}],')
  }
  
  fdat <- data.frame(nf, item, task, s, fitem, fmitem)
  
  fdat <- fdat %>% mutate_if(is.character, na_if, "")
  fdat <- fdat %>% select_if(all_na)
  colnames(fdat) <- 1:ncol(fdat)
  
  fdat <- t(apply(fdat, 1, move_NAs_to_end))
  fdat <- as.data.frame(fdat)
  
  fdat[is.na(fdat)] <- " "
  
  write.table(fdat, "MazeFiller.js", quote = FALSE, row.names = FALSE, col.names = FALSE)
  
}

JudgementExp <- function(
    Material = NULL,
    NumberOfCondition = NULL,
    NameOfExperimentalItem = NULL,
    StartFrom = NULL) {
  
  colnames(Material) <- paste0("c", 1:ncol(Material))
  
  sitem <- subset(Material, c1 == "s:")[-1]
  
  sitem <- sitem[, colSums(is.na(sitem)) != nrow(sitem)]
  
  sset <- nrow(sitem) / NumberOfCondition
  cond <- paste0("[[", '"', NameOfExperimentalItem, rep(1:NumberOfCondition, times = sset), '",')
  
  item <- paste0(rep(StartFrom:(sset + StartFrom - 1), each = NumberOfCondition), "],")
  
  task <- rep('"AcceptabilityJudgment",', nrow(sitem))
  s <- rep("{s:", nrow(sitem))
  
  for (i in 1:nrow(sitem)) {
    sitem$c2[i] <- paste0('"', sitem$c2[i])
    last_col <- max(which(sitem[i, ] != ""))
    sitem[i, last_col] <- paste0(sitem[i, last_col], '"}],')
  }
  
  sdat <- data.frame(cond, item, task, s, sitem)

  sdat <- sdat %>% mutate_if(is.character, na_if, "")
  sdat <- sdat %>% select_if(all_na)
  colnames(sdat) <- 1:ncol(sdat)
  
  sdat <- t(apply(sdat, 1, move_NAs_to_end))
  sdat <- as.data.frame(sdat)
  
  sdat[is.na(sdat)] <- " "
  
  write.table(sdat, "JudgementExp.js", quote = FALSE, row.names = FALSE, col.names = FALSE)

}

JudgementFiller <- function(
    Material = NULL,
    NameOfFillerItem = NULL,
    StartFrom = NULL) {
  
  colnames(Material) <- paste0("c", 1:ncol(Material))
  
  fitem <- subset(Material, c1 == "f:")[-1]
  
  fitem <- fitem[, colSums(is.na(fitem)) != nrow(fitem)]
  
  nf <- paste0("[[", '"', rep(NameOfFillerItem, nrow(fitem)), '",')
  
  item <- paste0(rep(StartFrom:(nrow(fitem) + StartFrom - 1)), "],")
  
  task <- rep('"AcceptabilityJudgment",', nrow(fitem))
  s <- rep("{s:", nrow(fitem))
  
  for (i in 1:nrow(fitem)) {
    fitem$c2[i] <- paste0('"', fitem$c2[i])
    last_col <- max(which(fitem[i, ] != ""))
    fitem[i, last_col] <- paste0(fitem[i, last_col], '"}],')
  }
  
  fdat <- data.frame(nf, item, task, s, fitem)
  
  fdat <- fdat %>% mutate_if(is.character, na_if, "")
  fdat <- fdat %>% select_if(all_na)
  colnames(fdat) <- 1:ncol(fdat)
  
  fdat <- t(apply(fdat, 1, move_NAs_to_end))
  fdat <- as.data.frame(fdat)
  
  fdat[is.na(fdat)] <- " "
  
  write.table(fdat, "JudgementFiller.js", quote = FALSE, row.names = FALSE, col.names = FALSE)
  
}




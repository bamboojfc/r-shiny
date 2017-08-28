library(shiny)
library(shinycssloaders)
library(stringi)
library(plyr)
library(DT)

######################## all functions ##########################

trigrams.prob = function(tokens, bigrams, trigrams, firstWord, secondWord, thirdWord){
  text2 <- paste(c(firstWord,secondWord),collapse = " ")
  text3 <- paste(c(firstWord,secondWord,thirdWord),collapse = " ")
  return((length(trigrams[trigrams==text3])+0.001)/(length(bigrams[bigrams==text2])+0.001))
}

bigrams.prob = function(tokens, bigrams, firstWord, secondWord){
  text <- paste(c(firstWord,secondWord),collapse = " ")
  return((length(bigrams[bigrams==text])+0.001) / (length(tokens[tokens==firstWord])+0.001))
}

unigrams.prob = function(tokens, query){
  numberOfQ <- length(tokens[tokens==query])
  return((numberOfQ+0.001) / (length(tokens[tokens!="STOP"])+0.001))
}

markov.trigrams = function(tokens, bigrams, trigrams, query){
  prob <- unigrams.prob(tokens, query[1])
  prob <- prob * bigrams.prob(tokens, bigrams, query[1], query[2])

  for(i in 3:length(query)){
    prob <- prob*trigrams.prob(tokens, bigrams, trigrams, query[i-2], query[i-1], query[i])
  }

  return(prob)
}

markov.bigrams = function(tokens, bigrams, query){
  prob <- unigrams.prob(tokens, query[1])
  for(i in 2:length(query)){
    prob <- prob * bigrams.prob(tokens, bigrams, query[i-1], query[i])
  }
  return(prob)
}

markov.unigrams = function(tokens, query){
  prob <- unigrams.prob(tokens, query[1])
  return(prob)
}

isAllUnseen = function(tokens, query){
  for(q in query){
    if(length(tokens[tokens==q&q!="STOP"])!=0){
      return(FALSE)
    }
  }
  return(TRUE)
}

createNGramAndCalProbs = function(d,query){
  prob.query <- 0
  query.tokens <- tokenize(query)
  len.q <- length(query.tokens)-1

  prob <- sapply(1:nrow(d), FUN = function(i){
    prob.query <- 0

    ### tokenize content into tokens
    t.title <- unlist(tokens.title[[i]])
    t.bigrams.title <- unlist(bigrams.title[[i]])
    t.trigrams.title <- unlist(trigrams.title[[i]])

    t.body <- unlist(tokens.body[[i]])
    t.bigrams.body <- unlist(bigrams.body[[i]])
    t.trigrams.body <- unlist(trigrams.body[[i]])

    if(length(t.title)!=0&&length(t.title)!=length(t.title[t.title=="STOP"])&&length(t.body)!=0&&length(t.body)!=length(t.body[t.body=="STOP"])){

      ## check if all tokens in query unseen in this doc's title
      if(!isAllUnseen(t.title, query.tokens)){
        ### markov process to calculate how much probability can this query be in this language
        ### (how much related to this doc)

        ## choose N-Gram probs calculation based on length of query.tokens
        if(len.q==1){
          prob.query <- markov.unigrams(t.title, query.tokens)
        }else if(len.q==2){
          prob.query <- markov.bigrams(t.title, t.bigrams.title, query.tokens)
        }else{
          prob.query <- markov.trigrams(t.title, t.bigrams.title, t.trigrams.title, query.tokens)
        }
      }

      ## check if all tokens in query unseen in this doc's body
      if(!isAllUnseen(t.body, query.tokens)){
        if(len.q==1){
          prob.query <- prob.query+markov.unigrams(t.body, query.tokens)
        }else if(len.q==2){
          prob.query <- prob.query+markov.bigrams(t.body, t.bigrams.body, query.tokens)
        }else{
          prob.query <- prob.query+markov.trigrams(t.body, t.bigrams.body, t.trigrams.body, query.tokens)
        }
      }
    }
    prob.query
  })

  return(prob)
}

tokenize = function(s){
  s <- tolower(s)
  s <- gsub(pattern="[[:punct:]]+\\s+|[[:punct:]]+$"," STOP ",s)
  tokens <- unlist(strsplit(s, '\\s+', perl=TRUE))
  tokens <- gsub(tokens, pattern = "^[[:punct:]]*", replacement = "")
  if(length(tokens)!=0){
    if(tokens[length(tokens)]!="STOP"){
      tokens <- c(tokens,'STOP')
    }
  }
  return(tokens[tokens!=""])
}

cleanData = function(d){
  ### remove html tag
  d <- gsub("<[^>]+>", "", d)
  d <- gsub("\u2028", "", d)
  ### remove html special character
  d <- gsub("&#?[a-zA-Z0-9];", "", d)
  ### unicode to ascii
  d <- stri_trans_general(d,"Latin-ASCII")
  ### replace ( ) [ ] with space
  d <- gsub(pattern = "\\]|\\[", " ", d)
  d <- gsub(pattern = "[()]", " ", d)
  ### remove \n
  d <- gsub(pattern = "\n", " ", d)
  ### separate camel case
  d <- gsub("([a-z])([A-Z])", "\\1 \\L\\2", d, perl = TRUE)
  return(d)
}

################## prepare all tokens from all documents ####################
### Don't forget to uncomment this section, when run in the first time (it may take about 20 mins) and comment them after that.

# ### get content from each document by read csv to dataframe
# path <- "C:/Users/U6063152/Documents/RScripts/data/"
# filename <- paste(path,"database.csv",sep = "")
# documents.df <- read.csv(filename)
# documents.df.tmp <- documents.df
# 
# 
# ### clean data
# documents.df$title <- cleanData(documents.df$title)
# documents.df$body <- cleanData(documents.df$body)
# documents.df$prob <- rep(0, nrow(documents.df))
# documents.df.clean <- documents.df
# 
# tokens.title <- list()
# bigrams.title <- list()
# trigrams.title <- list()
# tokens.body <- list()
# bigrams.body <- list()
# trigrams.body <- list()
# 
# tmp <- sapply(1:nrow(documents.df), FUN = function(i){
# 
#   s <- documents.df[i,]
# 
#   ### title ###
#   tokens <- tokenize(s$title)
#   tokens.title[[length(tokens.title)+1]] <<- list(tokens)
# 
#   tokens2 <- c(tokens[-1], "STOP")
#   bigrams <- paste(tokens, tokens2)
#   bigrams <- bigrams[!grepl(bigrams, pattern = "STOP STOP")]
#   bigrams.title[[length(bigrams.title)+1]] <<- list(bigrams)
# 
#   tokens3 <- c(tokens2[-1], "STOP")
#   trigrams <- paste(tokens, tokens2, tokens3)
#   trigrams <- trigrams[!grepl(trigrams, pattern = "STOP STOP")]
#   trigrams.title[[length(trigrams.title)+1]] <<- list(trigrams)
# 
#   ### body ###
#   tokens <- tokenize(s$body)
#   tokens.body[[length(tokens.body)+1]] <<- list(tokens)
# 
#   tokens2 <- c(tokens[-1], "STOP")
#   bigrams <- paste(tokens, tokens2)
#   bigrams <- bigrams[!grepl(bigrams, pattern = "STOP STOP")]
#   bigrams.body[[length(bigrams.body)+1]] <<- list(bigrams)
# 
#   tokens3 <- c(tokens2[-1], "STOP")
#   trigrams <- paste(tokens, tokens2, tokens3)
#   trigrams <- trigrams[!grepl(trigrams, pattern = "STOP STOP")]
#   trigrams.body[[length(trigrams.body)+1]] <<- list(trigrams)
#   
# 
# })
# 
# ### prepare authors & updaters info ###
# MAX_AUTHOR <- 24
# combine.link <- c()
# combine.name <- c()
# combine.status <- c()
# for(i in 0:MAX_AUTHOR){
#   col.link <- paste("authors.",i,".link",sep = "")
#   author <- as.character(documents.df[[col.link]])
#   combine.link <- c(combine.link, author[author!=""&!is.na(author)])
#   
#   col.author <- paste("authors.",i,".name",sep = "")
#   author <- as.character(documents.df[[col.author]])
#   combine.name <- c(combine.name, author[author!=""&!is.na(author)])
#   
#   col.status <- paste("authors.",i,".status",sep = "")
#   status <- as.character(documents.df[[col.status]])
#   combine.status <- c(combine.status, status[status!=""&!is.na(status)])
# }
# combine.df <- data.frame(combine.link, combine.name, combine.status)
# combine.df.unique <- unique.data.frame(combine.df)
# names(combine.df.unique) <- c("author.link","author.name","author.status")

#############################################################################

search = function(search.keyword=NULL){
  
  if(!is.null(search.keyword)&&search.keyword!=""){
    search.keyword <- cleanData(search.keyword)
    
    ### clear old search results and prepare for store calculated probs
    documents.df <- documents.df.clean
    
    ### Do this with each document (both title and body)
    documents.df$prob <- createNGramAndCalProbs(documents.df,search.keyword)
    
    ### rank documents
    documents.df <- documents.df[order(documents.df$prob,decreasing = TRUE),]
    if(nrow(documents.df)<100){
      documents.df.top <- documents.df[1:nrow(documents.df),]
    }else{
      documents.df.top <- documents.df[1:100,]
    }
    documents.df.top <- documents.df.top[documents.df.top$prob!=0,]
    
    if(nrow(documents.df.top)>0){
      ### new method to aggregate by authors and updaters
      
      authors.link <- c()
      for(i in 0:MAX_AUTHOR){
        col.link <- paste("authors.",i,".link",sep = "")
        author <- as.character(documents.df.top[[col.link]])
        authors.link <- c(authors.link, author[author!=""&!is.na(author)])
      }
      authors.link.unique <- unique(authors.link)
      
      result.df <- data.frame(author.link=character(), docs=numeric(), prob=numeric(), stringsAsFactors = FALSE)
      for(c in as.character(authors.link.unique)){
        ### c is a staff
        ## select only document includes author or updater == c from documents.df.top
        docs <- c()
        column.list <- which(apply(documents.df.top,2,function(x) any(x==c)))
        column.list <- (column.list-9)/3
        for(i in column.list){
          col.name <- paste("authors.",i,".link",sep = "")
          docs <- c(docs,as.character(documents.df.top[documents.df.top[[col.name]]==c,]$X_id))
        }
        docs <- unique(docs) #list of c's docs
        docs <- docs[!is.na(docs)]
        
        ### combine dataframe of c and document list to result
        doc <- documents.df.top[documents.df.top$X_id %in% docs, c('title','prob')]
        num <- nrow(doc)
        sum.prob <- sum(doc$prob)
        tmp <- data.frame(c, num, sum.prob)
        names(tmp) <- c('author.link', 'docs', 'probability')
        result.df <- rbind(result.df, tmp)
      }
      
      #map authors' name and status
      result.merge <- merge(result.df, combine.df.unique, by='author.link')
      
      #order author result by number of docs
      result.order <- result.merge[order(result.merge$docs, result.merge$probability, decreasing = TRUE),]
      row.names(result.order) <- 1:nrow(result.order)
      
      names(result.order) <- c('link', 'docs', 'probability', 'name', 'active')
      return(result.order)
    }
  }
  
  #return empty dataframe
  return(data.frame())
}

ui <- fluidPage(
  title = 'inTouch',
  fluidRow(
    column(6, align="center", offset = 3,
        tags$style(type="text/css", "#in { font-size: 80px;}"),
        tags$span( id = "in", "in"),
        tags$style(type="text/css", "#touch { padding-top: 100px; font-size: 80px; color:#EC6B18;}"),
        tags$span( id = "touch", "Touch"),
        HTML("
              <div align='center';>
                <input id='keyword' type='text' class='form-control' value='' placeholder='search keyword..' style='width: 80%; float:left; margin-right: 15px;'/>
                <button id='searchBtn' type='button' class='btn btn-default action-button' style='float:left;'>Search</button>
              </div>
             ")
    )
  ),
  tags$hr(),
  checkboxInput(inputId = "active", label = 'only activate user', value = TRUE),
  withSpinner(dataTableOutput(outputId = "results"), type = 6, color = '#EC6B18')
)

server <- function(input, output){
  df <- eventReactive(input$searchBtn, {
    search(input$keyword)
  })
  active <- reactive(input$active)
  
  output$results <- renderDataTable({
    d <- data.frame(df())
    if(nrow(d)!=0){
      if(active()){
        d <- d[d$active=='True',]
      }
      rownames(d) <- 1:nrow(d)
      d$name <- lapply(1:nrow(d), function(i){
        paste('<a href="',d[i,'link'],'">',d[i,'name'],"</a>")
      })
      datatable(d[,c('name','docs','active','probability')], rownames = TRUE)
    }else{
      datatable(data.frame('no results found'),colnames = c('results'))
    }
  })
}

###############################################################################

shinyApp(ui = ui, server = server)
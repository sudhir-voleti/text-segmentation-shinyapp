#------------------------------------------------------------------#
# 1. Load all necessary packages & Define Helper Functions
#------------------------------------------------------------------#
suppressPackageStartupMessages({
  library(shiny)
  library(text2vec)
  library(tm)
  library(tokenizers)
  library(wordcloud)
  library(slam)
  library(stringi)
  library(magrittr)
  library(tidytext)
  library(dplyr)
  library(tidyr)
  library(tools) # For file extension checking
})

# --- Helper Function: Text Cleaning ---
text.clean = function(x) {
  x  =  gsub("<.*?>", " ", x)
  x  =  iconv(x, "latin1", "ASCII", sub="")
  x  =  gsub("[^[:alnum:]]", " ", x)
  x  =  tolower(x)
  x  =  removeNumbers(x)
  x  =  stripWhitespace(x)
  x  =  gsub("^\\s+|\\s+$", "", x)
  return(x)
}

# --- Helper Function: Word Counting from DTM ---
dtm.word.count <- function(dtm) {
  if (is.null(dtm)) return(NULL)
  if (ncol(dtm) > 1000) {
    tst = round(ncol(dtm)/100)
    a = rep(tst,99)
    b = cumsum(a);rm(a)
    b = b[-which(b >= ncol(dtm))]
    b = c(0,b,ncol(dtm))
    
    ss.col = c(NULL)
    for (i in 1:(length(b)-1)) {
      tempdtm = dtm[,(b[i]+1):(b[i+1])]
      s = colSums(as.matrix(tempdtm))
      ss.col = c(ss.col,s)
    }
  } else {
    ss.col = colSums(as.matrix(dtm))
  }
  return(ss.col)
}

# --- Helper Function: Word Cloud Plotting ---
dtm.word.cloud <- function(count, max.words = 100, title = "Title"){
  if (is.null(count) || length(count) == 0) return(NULL)
  tsum <- if (class(count)[1] %in% c("DocumentTermMatrix", "simple_triplet_matrix")) {
    dtm.word.count(count)
  } else {
    count
  }
  
  if (!is.numeric(tsum)) stop("Input must be a numeric vector or a DTM.")
  
  wordcloud(names(tsum), tsum,
            scale = c(4, 1),
            min.freq = .01,
            max.words = max.words,
            colors = brewer.pal(8, "Dark2"))
  title(sub = title)
}

# --- Helper Function: Co-occurrence Graph ---
distill.cog.tcm = function(mat1, mattype = "DTM", title, s, k1){
  if (is.null(mat1) || nrow(mat1) == 0 || ncol(mat1) == 0) return(NULL)
  require(igraph)
  
  mat1 = as.matrix(mat1)
  
  if (mattype == "DTM"){
    mat1 = tcrossprod(t(mat1))
  }
  
  if (mattype == "TCM"){
    mat1 = mat1 + t(mat1)
  }
  
  ss.col = colSums(as.matrix(mat1))
  a = ss.col
  b = order(-a)
  
  mat2 = mat1[b, b]
  diag(mat2) =  0
  
  wc = NULL
  for (i1 in 1:min(s, nrow(mat2))){
    thresh1 = mat2[i1,][order(-mat2[i1, ])[k1]]
    mat2[i1, mat2[i1,] < thresh1] = 0
    mat2[i1, mat2[i1,] > 0 ] = 1
    word = names(mat2[i1, mat2[i1,] > 0])
    if (i1 < nrow(mat2)) {
      mat2[(i1+1):nrow(mat2), match(word,colnames(mat2))] = 0
    }
    wc = c(wc,word)
  }
  
  mat3 = mat2[match(wc, colnames(mat2)), match(wc, colnames(mat2))]
  ord = colnames(mat2)[which(!is.na(match(colnames(mat2), colnames(mat3))))]
  mat4 = mat3[match(ord, colnames(mat3)), match(ord, colnames(mat3))]
  
  graph <- graph.adjacency(mat4, mode = "undirected", weighted=T)
  graph = simplify(graph)
  V(graph)$color[1:min(s, length(V(graph)))] = "green"
  if (length(V(graph)) > s) {
    V(graph)$color[(s+1):length(V(graph))] = "pink"
  }
  
  graph = delete.vertices(graph, V(graph)[ degree(graph) == 0 ])
  
  plot(graph, layout = layout.kamada.kawai, main = title)
}

# --- Helper Function: DTM Creation ---
dtm.tcm.creator <- function(text, id = "", std.clean = TRUE, std.stop.words = TRUE,
                            stop.words.additional, bigram.encoding = TRUE, bigram.min.freq = 5,
                            min.dtm.freq = 2, skip.grams.window = 5) {
  
  stpw1 = readLines("data/stopwords.txt")
  stpw2 = tm::stopwords('english')
  stpw3  = unique(gsub("'"," ",c(stpw1,stpw2)))
  
  if (is.null(id) || id[1] == ""){ id = 1:length(text) }
  if (std.clean) { text = text.clean(text) }
  if (std.stop.words){
    stop.words.f = unique(c(stpw3,stop.words.additional))
    text = removeWords(text,stop.words.f)
    text = stripWhitespace(text)
  }
  
  tok_fun = word_tokenizer
  
  if (bigram.encoding){
    it_0 = itoken(text, tokenizer = tok_fun, ids = id, progressbar = F)
    vocab = create_vocabulary(it_0, ngram = c(2L, 2L))
    if (nrow(vocab) > 0){
      pruned_vocab = prune_vocabulary(vocab, term_count_min = bigram.min.freq)
      if (nrow(pruned_vocab) > 0) {
        replace_list = pruned_vocab$term[order(pruned_vocab$term_count, decreasing = T)]
        if (length(replace_list) > 0){
          text = paste("",text,"")
          for (term in replace_list){
            focal.term = gsub("_", " ",term)
            replacement.term = term
            text = gsub(paste("",focal.term,""),paste("",replacement.term,""), text)
          }
        }
      }
    }
  }
  
  it_m = itoken(text, tokenizer = tok_fun, ids = id, progressbar = F)
  vocab = create_vocabulary(it_m)
  pruned_vocab = prune_vocabulary(vocab, term_count_min = min.dtm.freq)
  vectorizer = vocab_vectorizer(pruned_vocab)
  dtm_m  = create_dtm(it_m, vectorizer)
  
  return(list(dtm = dtm_m))
}

#------------------------------------------------------------------#
# 2. UI Definition
#------------------------------------------------------------------#
# --- MODIFIED: Assign UI definition to a variable 'ui' ---
ui <- fluidPage(
  titlePanel(title=div(img(src="logo.png",align='right'),"Text Segmentation")),
  
  sidebarPanel(
    fileInput("file", "Upload Data File (.txt or .csv)"),
    uiOutput("column_selectors_ui"),
    hr(),
    h4("Analysis Parameters"),
    textInput("stopw", ("Enter stop words (comma-separated)"), value = "will,can"),
    selectInput("ws", "Weighing Scheme", c("weightTf","weightTfIdf"), selected = "weightTf"),
    sliderInput("freq", "Minimum Word Frequency:", min = 1,  max = 100, value = 2),
    sliderInput("max",  "Max Words in Wordcloud:", min = 1,  max = 300,  value = 50),
    numericInput("seg", "Number of Segments", 4),
    numericInput("nodes", "Central Nodes in Co-occurrence Graph", 4),
    numericInput("connection", "Max Connections with Central Node", 5)
  ),
  
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Overview",
                         h4(p("How to use this App")),
                         p("To use this app, upload a data file using the 'Browse' button. You can upload either:"),
                         tags$ul(
                           tags$li(strong("A .txt file:"), " Each line in the file will be treated as a separate document."),
                           tags$li(strong("A .csv file:"), " After uploading, you will be prompted to select the column containing your text documents and an optional ID column.")
                         ),
                         p("Once the file is uploaded, computations will run in the background. You can adjust the analysis parameters in the sidebar to refine the results."),
                         br(),
                         h4(p("Download Sample Text File")),
                         downloadButton('downloadData1', 'Download Ice Cream Flavors (.txt)'),br(),br(),
                         p("Please note that downloads work best in a web browser, not the RStudio viewer."),
                         img(src = "example1.png")
                ),
                tabPanel("TDM & Word Cloud",
                         h4("Term Document Matrix [Top 10 Terms, First 10 Docs]"),
                         verbatimTextOutput("dtmsummary"),
                         hr(),
                         h4("Word Cloud"),
                         plotOutput("wordcloud",height = 700, width = 700),
                         h4("Word Counts for Wordcloud"),
                         verbatimTextOutput("dtmsummary1")
                ),
                tabPanel("Segmentation - Summary",
                         h4("Principal Component Scree Plot"),
                         plotOutput("pcaplot",height = 600, width = 700),
                         h4("Segmentation Summary"),
                         verbatimTextOutput("summary")
                ),
                tabPanel("Segmentation - Word cloud", uiOutput("segplots")),
                tabPanel("Segmentation - Co-occurrence", uiOutput("segcoocrplots")),
                tabPanel("Segmentation - Data", br(), br(),
                         downloadButton('downloadData2', 'Download Segmentation Data'), br(), br(),
                         dataTableOutput("table0"))
    )
  )
)

#------------------------------------------------------------------#
# 3. Server Logic
#------------------------------------------------------------------#
# --- MODIFIED: Assign server function to a variable 'server' ---
server <- function(input, output, session) {
  set.seed(2092014)

  data_store <- reactiveValues(raw_data = NULL)

  observeEvent(input$file, {
    req(input$file)
    ext <- file_ext(input$file$name)
    if (ext == "csv") {
      data_store$raw_data <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    } else if (ext == "txt") {
      data_store$raw_data <- readLines(input$file$datapath)
    } else {
      data_store$raw_data <- NULL
      shinyjs::alert("Unsupported file type. Please upload a .csv or .txt file.")
    }
  })

  output$column_selectors_ui <- renderUI({
    req(input$file, data_store$raw_data)
    ext <- file_ext(input$file$name)
    if (ext != "csv") return(NULL) 

    column_names <- names(data_store$raw_data)
    tagList(
      selectInput("text_col", "Select Text Column", choices = column_names, selected = column_names[1]),
      selectInput("id_col", "Select Document ID Column (Optional)", choices = c("None (use row numbers)" = "none", column_names))
    )
  })

  prepared_data <- reactive({
    req(input$file, data_store$raw_data)
    ext <- file_ext(input$file$name)

    if (ext == "txt") {
      docs <- data_store$raw_data
      df <- data.frame(Doc.id = seq_along(docs), Document = docs, stringsAsFactors = FALSE)
    } else if (ext == "csv") {
      req(input$text_col)
      
      docs <- data_store$raw_data[[input$text_col]]
      
      doc_ids <- if (!is.null(input$id_col) && input$id_col != "none") {
        data_store$raw_data[[input$id_col]]
      } else {
        seq_along(docs)
      }
      df <- data.frame(Doc.id = doc_ids, Document = docs, stringsAsFactors = FALSE)
    } else {
      return(NULL)
    }
    
    return(df[df$Document != "" & !is.na(df$Document), ])
  })
  
  dtm_tcm <- reactive({
    req(prepared_data())
    
    textb <- prepared_data()$Document
    ids <- prepared_data()$Doc.id
    
    dtm.tcm <- dtm.tcm.creator(text = textb,
                              id = ids,
                              std.clean = TRUE,
                              std.stop.words = TRUE,
                              stop.words.additional = unlist(strsplit(input$stopw,",")),
                              bigram.encoding = TRUE,
                              min.dtm.freq = input$freq)
                              
    if (input$ws == "weightTf") {
      dtm = as.matrix(dtm.tcm$dtm)
    } else if (input$ws == "weightTfIdf"){
      model_tfidf = TfIdf$new()
      dtm = as.matrix(model_tfidf$fit_transform(dtm.tcm$dtm))
      
      tempd = dtm*0
      tempd[dtm > 0] = 1
      dtm = dtm + tempd
    }
    
    dtm = dtm[rowSums(dtm)>0, colSums(dtm)>0, drop=FALSE]
    return(list(dtm = dtm))
  })
  
  wordcounts <- reactive({
    req(dtm_tcm())
    dtm.word.count(dtm_tcm()$dtm)
  })
  
  output$wordcloud <- renderPlot({
    req(wordcounts())
    tsum <- wordcounts()
    tsum <- tsum[order(tsum, decreasing = T)]
    dtm.word.cloud(count = tsum, max.words = input$max, title = 'Term Frequency Wordcloud')
  })
  
  output$dtmsummary  <- renderPrint({
    req(dtm_tcm(), wordcounts())
    sortedobj = dtm_tcm()$dtm[, order(wordcounts(), decreasing = T), drop=FALSE]
    
    rows_to_show <- min(10, nrow(sortedobj))
    cols_to_show <- min(10, ncol(sortedobj))
    
    (t(sortedobj[1:rows_to_show, 1:cols_to_show]))
  })
  
  output$dtmsummary1  <- renderPrint({
    req(wordcounts())
    data.frame(Counts = wordcounts()[order(wordcounts(), decreasing = T)][1:input$max])
  })
  
  norm_eucl <- function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5)
  
  m_norm <- reactive({
    req(dtm_tcm())
    norm_eucl(dtm_tcm()$dtm)
  })
  
  tdm <- reactive({
    req(dtm_tcm())
    t(dtm_tcm()$dtm)
  })
  
  data.pca <- reactive({
    req(m_norm())
    prcomp(m_norm(), center = TRUE, scale. = TRUE)
  })
  
  output$pcaplot <- renderPlot({
    req(data.pca())
    plot(data.pca(), type = "l"); abline(h=1)
  })
  
  clust <- reactive({
    req(m_norm())
    set.seed(19890201)
    kmeans(m_norm(), input$seg)
  })
  
  da <- reactive({
    req(clust(), tdm())
    da <- data.frame(as.numeric(colnames(tdm())), clust()$cluster)
    colnames(da) <- c("Doc.id", "Seg.Membership")
    return(da)
  })
  
  output$summary  <- renderPrint({
    req(clust())
    fit <- clust()
    list(Count = table(fit$cluster), Segment.Membership = as.character(fit$cluster))
  })
  
  output$segplots <- renderUI({
    req(clust())
    plot_output_list <- lapply(1:input$seg, function(i) {
      plotname <- paste("plot", i, sep="")
      plotOutput(plotname, height = 700, width = 700)
    })
    do.call(tagList, plot_output_list)
  })
  
  max_plots = 20
# CORRECTED CODE BLOCK
  for (i in 1:max_plots) {
    local({
      my_i <- i
      plotname <- paste("plot", my_i, sep="")
      
      output[[plotname]] <- renderPlot({
        # Ensure all reactive dependencies are ready
        req(clust(), tdm())
        cl <- clust()
        
        # Don't try to render a plot for a segment that doesn't exist
        if (my_i > length(cl$size)) return(NULL) 
        
        # Calculate percentages
        pct <- round((cl$size/sum(cl$size))*100, 2)
        
        # Subset the term-document matrix for the current segment
        stdm <- tdm()[, (cl$cluster == my_i), drop = FALSE]
        
        # Calculate word frequencies for the segment
        m <- as.matrix(stdm)
        v <- sort(rowSums(m), decreasing = TRUE)
        
        # Filter for words that actually appear in this segment
        v <- v[v > 0]
        
        # If no words are left after filtering, don't generate a plot
        if (length(v) == 0) return(NULL) 

        # Directly use the named vector for the word cloud
        # names(v) provides the words, v provides the frequencies.
        # Use max.words to control the number of words shown.
        wordcloud(words = names(v), 
                  freq = v, 
                  max.words = 40, # Limit to top 40 words
                  scale=c(4,1), 
                  random.order=FALSE, 
                  random.color=FALSE, 
                  colors=brewer.pal(8, "Dark2"))

        # Add title and box
        mtext(paste("Segment", my_i, "(", pct[my_i], "%)"), side = 3, line = 2, cex=2)
        box(lty = '11', col = 'black')
      })
    })
  }
  
  output$segcoocrplots <- renderUI({
    req(clust())
    plot_output_list <- lapply(1:input$seg, function(i) {
      plotname <- paste("plot1", i, sep="")
      plotOutput(plotname, height = 700, width = 700)
    })
    do.call(tagList, plot_output_list)
  })
  
  for (i in 1:max_plots) {
    local({
      my_i <- i
      plotname <- paste("plot1", my_i, sep="")
      output[[plotname]] <- renderPlot({
        req(clust(), tdm())
        cl <- clust()
        if (my_i > length(cl$size)) return(NULL)
        
        pct <- round((cl$size/sum(cl$size))*100, 2)
        stdm <- tdm()[, (cl$cluster == my_i), drop = FALSE]
        
        distill.cog.tcm(t(stdm), "DTM", "", input$nodes, input$connection)
        mtext(paste("Segment", my_i, "(", pct[my_i], "%)"), side = 3, line = 2, cex=2)
        box(lty = '11', col = 'black')
      })
    })
  }
  
  output$downloadData1 <- downloadHandler(
    filename = function() { "Ice_Cream_Flavors.txt" },
    content = function(file) {
      writeLines(readLines("data/Ice_Cream_Flavors.txt"), file)
    }
  )
  
  output$downloadData2 <- downloadHandler(
    filename = function() { "Segmentation_file.csv" },
    content = function(file) {
      write.csv(da(), file, row.names=F)
    }
  )
  
  output$table0 <- renderDataTable({
    da()
  })
}

# --- ADDED: This line creates the Shiny app object and is ESSENTIAL for app.R ---
shinyApp(ui = ui, server = server)

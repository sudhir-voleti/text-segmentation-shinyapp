#################################################
#               Text Segmentation               #
#################################################

shinyServer(function(input, output,session) {
  set.seed=2092014   

dataset <- reactive({
    if (is.null(input$file)) {return(NULL)}
      else {
        Document = readLines(input$file$datapath)
        Doc.id=seq(1:length(Document))
        calib = data.frame(Doc.id,Document)
        calib = calib[calib$Document != "",]
        return(calib)}
      })

dtm_tcm =  reactive({
  
  textb = dataset()$Document
  ids = dataset()$Doc.id

  dtm.tcm = dtm.tcm.creator(text = textb,
                            id = ids,
                            std.clean = TRUE,
                            std.stop.words = TRUE,
                            stop.words.additional = unlist(strsplit(input$stopw,",")),
                            bigram.encoding = TRUE,
                            # bigram.min.freq = 20,
                            min.dtm.freq = input$freq,
                            skip.grams.window = 10)
  if (input$ws == "weightTf") {
    dtm = as.matrix(dtm.tcm$dtm)  
  } 
  
  if (input$ws == "weightTfIdf"){
    dtm = as.matrix(transform_tfidf(dtm.tcm$dtm))
    tempd = dtm*0
    tempd[dtm > 0] = 1
    dtm = dtm + tempd
      }
  dtm = dtm[rowSums(dtm)>0,colSums(dtm)>0]
  
  # tcm = dtm.tcm$tcm
  dtm_tcm_obj = list(dtm = dtm)#, tcm = tcm)
})

wordcounts = reactive({
  
  return(dtm.word.count(dtm_tcm()$dtm))
  
})

output$wordcloud <- renderPlot({
  tsum = wordcounts()
  tsum = tsum[order(tsum, decreasing = T)]
  dtm.word.cloud(count = tsum,max.words = input$max,title = 'Term Frequency Wordcloud')
  
      })
      
output$cog.dtm <- renderPlot({
  
  distill.cog.tcm(mat1=dtm_tcm()$dtm, # input TCM MAT
                  mattype = "DTM",
                  title = "COG from DTM Adjacency", # title for the graph
                  s=input$nodes,    # no. of central nodes
                  k1 = input$connection)  # No. of Connection with central Nodes
      })

# output$cog.tcm <- renderPlot({
#   
# distill.cog.tcm(mat1=dtm_tcm()$tcm, # input TCM MAT,
#                   mattype = "TCM",
#                   title = "TCM from glove algorithm - Graph ", # title for the graph
#                   s=input$nodes,    # no. of central nodes
#                   k1 = input$connection)  # No. of Connection with central Nodes
#   
#       })
        

output$dtmsummary  <- renderPrint({
      if (is.null(input$file)) {return(NULL)}
  else {
    sortedobj = dtm_tcm()$dtm[,order(wordcounts(), decreasing = T)]
    (t(sortedobj[1:10,1:10]))
  }
      })

output$dtmsummary1  <- renderPrint({
  if (is.null(input$file)) {return(NULL)}
  else {
    data.frame(Counts = wordcounts()[order(wordcounts(), decreasing = T)][1:input$max])
  }
})

#--------------------------------------------------#

norm_eucl <- function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5)

m_norm = reactive({
  m_norm <- norm_eucl(dtm_tcm()$dtm)
  return(m_norm)
})

tdm = reactive({
  t(dtm_tcm()$dtm)
})
data.pca <- reactive({prcomp(m_norm(),center = TRUE,scale. = TRUE)})

output$pcaplot <- renderPlot({
  if (is.null(input$file)) {return(NULL)}
  else {
    
    plot(data.pca(), type = "l"); abline(h=1)
  }
})

clust = reactive({
  set.seed(19890201)
  cl <- kmeans(m_norm(), input$seg)
  return(cl)
})

da = reactive({
  da = data.frame(as.numeric(colnames(tdm())),clust()$cluster)  
  colnames(da) = c("Doc.id","Seg Membership")
  return(da)
})


output$summary  <- renderPrint({
  if (is.null(input$file)) {return(NULL)}
  else {
    
    fit = clust()
    Segment.Membership = as.character(fit$cluster)
    # clustmeans = aggregate(Dataset2(),by = list(Segment.Membership), FUN = mean)
    Summary = list( Count = table(Segment.Membership),Segment.Membership = Segment.Membership )
    Summary
  }
})


output$segplots <- renderUI({
  if (is.null(input$file)) {return(NULL)}
  else {
    
    plot_output_list <- lapply(1:input$seg, function(i) {
      plotname <- paste("plot", i, sep="")
      plotOutput(plotname, height = 700, width = 700)
    })
    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, plot_output_list)
  }
})

# Call renderPlot for each one. Plots are only actually generated when they
# are visible on the web page.
max_plots = 20

for (i in 1:max_plots) {
  # Need local so that each item gets its own number. Without it, the value
  # of i in the renderPlot() will be the same across all instances, because
  # of when the expression is evaluated.
  local({
    
    my_i <- i 
    plotname <- paste("plot", my_i, sep="")
    
    output[[plotname]] <- renderPlot({
      cl = clust()
      pct = round((cl$size/sum(cl$size))*100,2)
      
      stdm = tdm()[,(cl$cluster == my_i)]
      m  = as.matrix(stdm)
      v = sort(rowSums(m), decreasing = TRUE)
      v = data.frame(v[v > 0 ])
      if (nrow(v) >= 40) {n = 40}
      else {n = nrow(v)}
      top_word = as.matrix(v[1:n,])
      row.names(top_word) = row.names(v)[1:n]
      
      # title(main =paste("Segment ",my_i))
      wordcloud(rownames(top_word), top_word,  scale=c(4,1), 1, random.order=FALSE, random.color=FALSE, colors=brewer.pal(8, "Dark2"));
      mtext(paste("Segment",my_i, "(",pct[my_i],"%)"), side = 3, line = 2, cex=2)
      
      box(lty = '11', col = 'black')
    })
  })
}

#-----------------------------------------

output$segcoocrplots <- renderUI({
  if (is.null(input$file)) {return(NULL)}
  else {
    
    plot_output_list <- lapply(1:input$seg, function(i) {
      plotname <- paste("plot1", i, sep="")
      plotOutput(plotname, height = 700, width = 700)
    })
    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, plot_output_list)
  }
})

# Call renderPlot for each one. Plots are only actually generated when they
# are visible on the web page.
max_plots = 20

for (i in 1:max_plots) {
  # Need local so that each item gets its own number. Without it, the value
  # of i in the renderPlot() will be the same across all instances, because
  # of when the expression is evaluated.
  local({
    
    my_i <- i 
    plotname <- paste("plot1", my_i, sep="")
    
    output[[plotname]] <- renderPlot({
      cl = clust()
      pct = round((cl$size/sum(cl$size))*100,2)
      
      stdm = tdm()[,(cl$cluster == my_i)]

      
      distill.cog.tcm(t(stdm),
                      mattype = "DTM",
                      "",
                  input$nodes,
                  input$connection)
      
      mtext(paste("Segment",my_i, "(",pct[my_i],"%)"), side = 3, line = 2, cex=2)
      
      box(lty = '11', col = 'black')
    })
  })
}


output$downloadData1 <- downloadHandler(
    filename = function() { "Ice_Cream_Flavers.txt" },
    content = function(file) {
      writeLines(readLines("data/Ice_Cream_Flavers.txt"), file)
    }
  )

output$downloadData2 <- downloadHandler(
  filename = function() { "Segmentation file.csv" },
  content = function(file) {
    write.csv(da(), file, row.names=F)
  })


})

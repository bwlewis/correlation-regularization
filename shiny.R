library("shiny")
library("quantmod")
library("networkD3")
library("corpcor")

# Pull down the stock return data from my web site. Replace this
# with the quantmod-based data collection procedure if you like.
con = url("http://illposed.net/returns.rdata")
load(con)
close(con)


runApp(list(

  ui = pageWithSidebar( # See ?pageWithSidebar for help
    headerPanel("Stock return series clustering"),
    sidebarPanel(
      sliderInput("lambda", div(HTML("&lambda;")), min=0.0, max=1, value=0.9, step=0.01),
      sliderInput("threshold", "threshold", min=0.0, max=1, value=0.9, step=0.01)
    ),
    mainPanel(
      simpleNetworkOutput("network")
    )
  ),

  server = function(input, output, session)
  {

    output$network <- renderSimpleNetwork({
      Sr <- cor.shrink(returns,lambda=input$lambda)
      Pr <- solve(Sr,diag(rep(1,nrow(Sr))))
      Qr <- Pr*(abs(Pr)>quantile(abs(Pr),probs=input$threshold))
      colnames(Qr) <- rownames(Qr) <- symbols
      edges <- which(Qr!=0, arr.ind=TRUE) # Adjaceny graph edge list
      links <- data.frame(source=symbols[edges[,2]], target=symbols[edges[,1]])
      names(sector_assignment) <- symbols
      N <- length(levels(sector_assignment))
      sector_palette <- substr(rainbow(N), 1, 7)
      vertex_colors <- sector_palette[as.integer(sector_assignment[unique(Reduce(c,t(links)))])]
      simpleNetwork(links, fontSize=16, textColour="#000011",
              linkColour="#bbbbbb", nodeColour=vertex_colors,
              charge=-250, nodeClickColour=NULL)
  })

  }
))

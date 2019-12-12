
## importing required libraries
library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashTable)
library(tidyverse)
library(plotly)
library(ggplot2)
library(readxl)
library("scales")
library(ggthemes)

theme_set(theme_bw())

app <- Dash$new(external_stylesheets = "https://codepen.io/chriddyp/pen/bWLwgP.css")

colors <- list(
  text = '#0013a3'
)

textStyle = list(
  textAlign = 'center',
  color = colors$text
)

# importing wrangled dataset
df <- read_csv("https://raw.githubusercontent.com/UBC-MDS/DSCI-532_group-211_R-dash/master/data/tab1.csv")
df2 <- read_csv("https://raw.githubusercontent.com/UBC-MDS/DSCI-532_group-211_R-dash/master/data/tab2.csv")
df$company <- factor(df$company)

# Dataset intro table
table <- dashDataTable(
            id='data-table',
              style_table = list(
              maxHeight = '200',
              overflowY = 'scroll'
            ),
          columns = map(colnames(df), 
                   function(colName){
                     list(
                       id = colName,
                       name = colName
                     )
                   }),
          data=df[c(1,2,3,123,124,125,246,247,248,369,370,371,437,438,439), ]    

                )

# stock history plot
make_graph1 <- function(df){
    plot1_tab1 <- df %>%
        ggplot(aes(x = date, y = price, group = company, color = company)) +
        geom_line() +
        ggtitle("Stock price change from 2000 to 2010") +
        labs(x = "Date",
            y = "Stock Price") +
        theme(plot.title = element_text(hjust = 0.5, vjust = 2))
ggplotly(plot1_tab1, dynamicTicks = TRUE, tooltip = c("y", "x", "group")) %>%
  rangeslider() %>%
  layout(hovermode = "x")
}


# monthly price change plot
make_graph2 <- function(df){

plot2_tab1 <- df %>% 
    ggplot(aes(x = date, y = monthly_return, fill = (monthly_return > 0))) + 
    geom_bar(stat = "identity") +
    labs(x = "\n \n Date",
         y = "Monthly Change %",
         title = "Monthly price changes between 2000 and 2010") +
    scale_fill_manual(values = c("orange", "royalblue")) +
    theme(legend.title = element_blank(),
          legend.position = "none",
          plot.title = element_text(hjust = 0.5, vjust = 2)) +
    facet_wrap(~ company, nrow = 2) 
    # panel_border() +
    # background_grid()

    ggplotly(plot2_tab1)

                       }


  #plot for tab3

  make_graph3 <- function(df2){


    plot_tab2 <-  df2 %>%
        ggplot(aes(x = date, y = inv_value, group = company, color = company)) +
        geom_line() +
        ggtitle("Investment value from August 2004 to March 2010") +
        labs(x = "Date",
            y = "Investment Value $USD")
            ggplotly(plot_tab2)
  }




# Now we define the graph as a dash component using generated figure
graph1 <- dccGraph(
  id = 'history-graph',
  figure=make_graph1(df) # gets initial data using argument defaults
)

graph2 <- dccGraph(
  id = 'monthly-graph',
  figure=make_graph2(df) # gets initial data using argument defaults
)

graph3 <- dccGraph(
  id = 'investment-graph',
  figure=make_graph3(df2) # gets initial data using argument defaults
)
# stock companies' dropdown

stocksDropdown <- dccDropdown(
  id = 'stocks-dropdown',
  
  options = map(
    levels(df$company), function(x){
      list(label=x, value=x)
    }),
  value = levels(df$company), #Selects all by default
  style = list(width = 500),
  multi = TRUE
)



## slider for tab3
dates <- c(
         '2004-08-01', '2005-02-01', '2005-08-01', '2006-02-01', '2006-08-01',
         '2007-02-01',  '2007-08-01','2008-02-01',  '2008-08-01','2009-02-01',  '2009-08-01',
         '2010-02-01', '2010-03-01')
# date_mark = {i : dates[i] for i in range(0,13)}

yearMarks <- map(dates, as.character)
# names(yearMarks) <- dates
yearSlider <- dccSlider(
  id = "year",
  marks = yearMarks,
  min = 0,
  max = 12,
  
 
  value = 12
)

app$layout(
  htmlDiv(
    list(
      htmlH1("Stock Price Data Analysis"),
      dccTabs(id="tabs-example", value='tab-1',children=list(


        # tab1 for data intro
    dccTab(label = "About our data", htmlDiv(list(
      htmlH1("Dataset Introduction"),
      htmlP("The dataset we are using is the  Stocks  data from the vega-datasets. 
            The dataset has 560 observations in total. "),

                # addding dataset intro table to tab1
            table,

            
            htmlP("There are 5 companies in total, and they are Microsoft, Amazon, IBM, Google, and Apple."),
            htmlP("The date column lists out the date when the stock price was recorded.
            The value of the date column ranges from January 1, 2000 to March 1, 2010. The date range is the same for Microsoft, Amazon, IBM, and Apple. Each of them has 123 observations in the dataset. 
            Since Google held its IPO in August, 2004, the record for Google started from August 1, 2004. 
            Therefore, there are 68 observations for Google."),
            htmlP(" The price column lists out the price of that stock on the recorded date."),
            htmlP(" The purpose of this app is to help people form a better view of stock price fluctuations 
                        and long-term investment gains.")
   
            )
            )), 

    # tab 2


  
    dccTab(label='Stock trends', value = 'tab-2', htmlDiv(list(

        htmlH1("Price History"),
                htmlH2("From 2000 to 2010, Apple's stock price increased 760%." ),
                htmlH3("In this interactive chart below, you can visualize how the stocks of 5 major tech companies changed between 2000 and 2010." ),
                htmlP("Use the dropdown window to select the company you want to explore. Use the slide bar down the graph to select the time range.") ,
                stocksDropdown, 
                #space
                htmlIframe(height=15, width=10, style=list(borderWidth = 0)),
                # History chart
                graph1, 
                #space
                htmlIframe(height=15, width=10, style=list(borderWidth = 0)),
                # monthly chart
                graph2
      )
              )


    ),

    dccTab(label = "Investment Value" ,htmlDiv(list(
        htmlH1("How much will I gain?"),
        htmlH3("If I invested $10,000 in one of the companies in August 2004, how much will my investment worth in later days for each company?"),
        htmlP("Use the year slider bar to select the time range and find out the investment value."), 
                       
            
    # range slider for selecting time range
        htmlP(yearSlider),
  
            
            # To add some space
              
              htmlIframe(height=15, width=10, style=list(borderWidth = 0)), #space
            # adding investment plot

            graph3,
             
                htmlH1("Why Apple has the highest investment value?"),
                htmlP("If you are curious why Apple has the highest investment value while Google has the highest price in the historial price chart you have seen in the previous tab, let's see the math here."), 
                     
                htmlP("In August 2004, Google's stock price was $102.37. With $10,000, I can buy 10,000/102.37 = 97.68 shares."), 
                    
                htmlP("In March 2010, Google's stock price was $560.19. Then my total investment value is 560.19 * 97.68 shares = $54,722.08."), 
                    
                htmlP("On the other hand, in August 2004, Apple's stock price was $17.25. With $10,000, I can buy 10,000/102.37 = 579.71 shares."), 
                    
                htmlP("In March 2010, Apple's stock price was $223.02. Then my total investment value is  223.02 579.71 * shares = $129,286.95."), 
                   
                htmlH4("Clearly, $129,286.95 worth much more than $54,722.08. You would earn much more if you picked Apple!"), 
                       
                htmlH3("Between 2004 and 2010, Google's stock price only increased 447.91%, but Apple increased 1192.9%."), 
                    
                htmlP("It is this high growth that drags up Apple's investment value."), 
                    
                htmlH2("In investment, growth is more important than price!"), 
                    
                htmlP("Hope this answers your question and gives you some insights on investment."), 
                    
                
                # To add some space
                htmlIframe(height=15, width=10, style=list(borderWidth = 0)) #space


    )
    ))

)
)
)
  )
)


# interactivity of dropdown with chart 1

app$callback(
 
  output=list(id = 'history-graph', property='figure'),
  
  params=list(input(id = 'stocks-dropdown', property='value')
              ),
 
  function(dropdown_value) {
    df <- df %>% 
            filter(company %in% dropdown_value )
    make_graph1(df)
  })

  # interactivity of dropdown with chart 2

app$callback(
  output=list(id = 'monthly-graph', property='figure'),
 
  params=list(input(id = 'stocks-dropdown', property='value')
              ),

  function(dropdown_value) {
    df <- df %>% 
            filter(company %in% dropdown_value )
    make_graph2(df)
  })


    # interactivity of slider with tab3

app$callback(
  output=list(id = 'investment-graph', property='figure'),
 
  params=list(input(id = 'year', property='value')
              ),

  function(year_range) {

    df2 <- df2 %>% 
            filter(date >= '2004-08-01' &  date <= dates[year_range]) 
    make_graph3(df2)
  })


#app$run_server()
app$run_server(host = "0.0.0.0", port = Sys.getenv('PORT', 8050))

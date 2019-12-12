

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

textStyle1 = list(
  textAlign = 'center',
  color = colors$text
)

textStyle2 = list(
  textAlign = 'center'
)

# importing wrangled dataset
df <- read_csv("https://raw.githubusercontent.com/UBC-MDS/DSCI-532_group-211_R-dash/master/data/tab1.csv")
df2 <- read_csv("https://raw.githubusercontent.com/UBC-MDS/DSCI-532_group-211_R-dash/master/data/tab2.csv")
df$company <- factor(df$company)


# prepare dataframe for data table in tab1
all_companies <- unique(df$company)
make_table <- function(years=c(2000, 2010), 
                       companies = all_companies){
  
  df %>%
    filter(year >= years[1] & year <= years[2]) %>%
    filter(company %in% companies) %>%
    df_to_list()
  
}

# NEW: Added table that can be sorted!
# Table that can be sorted by column
table <- dashDataTable(
  id = "stock-table",
  # these make the table scrollable
  fixed_rows = list(headers = TRUE, data = 0),
  style_table = list(
    maxHeight = '500',
    overflowY = 'scroll'
  ),
  columns = map(colnames(df), 
                   function(colName){
                     list(
                       id = colName,
                       name = colName
                     )
                   }),
  data = df,
  sort_action="native"
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

# stock companies' dropdown for tab1
stocksDropdown1 <- dccDropdown(
  id = "stocks-dropdown1",
  # map/lapply can be used as a shortcut instead of writing the whole list
  # especially useful if you wanted to filter by country!
  options = map(
    levels(df$company), function(x){
      list(label=x, value=x)
    }),
  value = levels(df$company), #Selects all by default
  multi = TRUE
)
# stock companies' dropdown for tab2
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

## slider for tab1
yearMarks <- map(unique(df$year), as.character)
names(yearMarks) <- unique(df$year)
yearSlider1 <- dccRangeSlider(
  id = "year1",
  marks = yearMarks,
  min = 2000,
  max = 2010,
  step = 1,
  value = list(2000, 2010)
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
  
 
  value = list(2000, 2010)
)


app$layout(
  htmlDiv(
    list(
      htmlH1("Stock Price Data Analysis", style = textStyle2),
      dccTabs(id="tabs-example", value='tab-1',children=list(


        # tab1 for data intro
    dccTab(label = "About our data", htmlDiv(list(
      htmlH1("Dataset Introduction", style = textStyle1),
      htmlP("The dataset we are using is the  Stocks  data from the vega-datasets. 
            The dataset has 560 observations in total. ", style = list('font-size' = '17px')),
      htmlP("There are 5 companies in total, and they are Microsoft, Amazon, IBM, Google, and Apple. 
      You can use the dropdown window to select the companies.", style = list('font-size' = '17px')),
      
            stocksDropdown1,
    #space
       htmlIframe(height=15, width=10, style=list(borderWidth = 0)),
      htmlP("The date column lists out the date when the stock price was recorded.
            The value of the date column ranges from January 1, 2000 to March 1, 2010. The date range is the same for Microsoft, Amazon, IBM, and Apple. Each of them has 123 observations in the dataset. 
            Since Google held its IPO in August, 2004, the record for Google started from August 1, 2004. 
            Therefore, there are 68 observations for Google.", style = list('font-size' = '17px')), 
        htmlP("You can use the slider bar to select the date range.", style = list('font-size' = '17px')),   
            yearSlider1,
       #space
       htmlIframe(height=35, width=10, style=list(borderWidth = 0)),      
                # addding dataset intro table to tab1
            table,
       #space
       htmlIframe(height=35, width=10, style=list(borderWidth = 0)), 
            
            
            
            htmlP(" The price column lists out the price of that stock on the recorded date.", style = list('font-size' = '17px')),
            htmlP(" We created the monthly_return column, which is the monthly percentage changes 
            in stock prices compare to the previous month.", style = list('font-size' = '17px')),
            htmlP(" The volatility column shows the stardard deviation of stock prices within a year.", style = list('font-size' = '17px')),
            htmlP(" The purpose of this app is to help people form a better view of stock price fluctuations 
                        and long-term investment gains.", style = list('font-size' = '17px')),
      #space
       htmlIframe(height=45, width=10, style=list(borderWidth = 0)) 
   
            )
            )), 

    # tab 2


  
    dccTab(label='Stock trends', value = 'tab-2', htmlDiv(list(

        htmlH1("Price History", style = textStyle1),
                htmlH2("From 2000 to 2010, Apple's stock price increased 760%.", style = textStyle2),
                htmlH3("In this interactive chart below, you can visualize how the stocks of 5 major tech companies changed between 2000 and 2010." ),
                htmlP("Use the dropdown window to select the company you want to explore.
                Use the slide bar down the graph to select the time range.", style = list('font-size' = '17px')) ,
                stocksDropdown, 
                #space
                htmlIframe(height=15, width=10, style=list(borderWidth = 0)),
                # History chart
                graph1, 
                #space
                htmlIframe(height=15, width=10, style=list(borderWidth = 0)),
                # monthly chart
                graph2,
                htmlIframe(height=15, width=10, style=list(borderWidth = 0))
      )
              )


    ),

    dccTab(label = "Investment Value" ,htmlDiv(list(
        htmlH1("How much will I gain?", style = textStyle1),
        htmlH3("If I invested $10,000 in one of the companies in August 2004, how much will my investment worth in later days for each company?"),
        htmlP("Use the year slider bar to select the time range and find out the investment value."), 
                       
            
    # range slider for selecting time range
        htmlP(yearSlider),
  
            
            # To add some space
              
              htmlIframe(height=15, width=10, style=list(borderWidth = 0)), #space
            # adding investment plot

            graph3,
             
                htmlH1("Why Apple has the highest investment value?", style = textStyle1),
                htmlP("If you are curious why Apple has the highest investment value 
                while Google has the highest price in the historial price chart you have seen in the previous tab, 
                let's see the math here.", style = list('font-size' = '17px')), 
                     
                htmlP("In August 2004, Google's stock price was $102.37. 
                With $10,000, I can buy 10,000/102.37 = 97.68 shares.", style = list('font-size' = '17px')), 
                    
                htmlP("In March 2010, Google's stock price was $560.19. 
                Then my total investment value is 560.19 * 97.68 shares = $54,722.08.", style = list('font-size' = '17px')), 
                    
                htmlP("On the other hand, in August 2004, Apple's stock price was $17.25. 
                With $10,000, I can buy 10,000/102.37 = 579.71 shares.", style = list('font-size' = '17px')), 
                    
                htmlP("In March 2010, Apple's stock price was $223.02. 
                Then my total investment value is  223.02 579.71 * shares = $129,286.95.", style = list('font-size' = '17px')), 
                   
                htmlH4("Clearly, $129,286.95 worth much more than $54,722.08. 
                You would earn much more if you picked Apple!"), 
                       
                htmlH3("Between 2004 and 2010, Google's stock price only increased 447.91%, but Apple increased 1192.9%."), 
                    
                htmlP("It is this high growth that drags up Apple's investment value.", style = list('font-size' = '17px')), 
                    
                htmlH2("In investment, growth is more important than price!", style = list(color = '#0013a3')), 
                    
                htmlP("Hope this answers your question and gives you some insights on investment.", style = list('font-size' = '17px')), 
                    
                
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

app$callback(
  #update data of stock-table
  output=list(id = 'stock-table', property='data'),
  params=list(input(id = 'year1', property='value'),
              input(id = 'stocks-dropdown1', property='value')),
  function(year_value, company_value) {
    make_table(year_value, company_value)
  })


#app$run_server()
app$run_server(host = "0.0.0.0", port = Sys.getenv('PORT', 8050))

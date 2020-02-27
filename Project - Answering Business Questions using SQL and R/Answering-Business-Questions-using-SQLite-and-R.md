---
title: "Answering Business Questions using SQLite and R"
author: "Philipp Pfister"
date: "22 Feb 2020"
output:
    html_document:
      self_contained: false
      keep_md: true
---

In this self solved project, which makes part of **[dataquest.io Data Analyst R Track](https://www.dataquest.io/path/data-analyst-r/)**, I will analyze a database called **Chinook.db**. The Chinook database contains information about various elements in a fictional digital music shop, such as artists, songs, albums, as well as the shop's employees, customers, and the customers' purchases. All this information is contained in 11 tables, which are:

1. **employee**
2. **customer**
3. **invoice**
4. **invoice_line**
5. **playlist**
6. **playlist_track**
7. **track**
8. **artist**
9. **album**
10. **media_type**
11. **genre**

See the database schema, with all variables and Primary Keys **[here](http://bit.ly/2uk9243)**.

The purpose of this project, is to get more familiar the use of intermediare SQL functions like subqueries, advanced joins like UNION, INTERSECT or the use of WITH statements, to finally answer business questions. The included database file can be found **[here](https://github.com/Philbarboza/Personal-Case-Studies/tree/master/Project%20-%20Answering%20Business%20Questions%20using%20SQL%20and%20R)**.


# Introduction

In the introduction I will:

1. Install and load necessary packages
3. Create helper function and verify included tables

#### 1. Installing and loading packages

```r
library(RSQLite)
library(DBI)
library(tidyverse)
library(Hmisc)
library(png)
library(knitr)
library(plotly)
```

#### 2. Create helper function

This helper function can be thought of as any function that performs a small operation in the context of a greater process. I'll call this helper function run_query() and will be used frequently throughout the project, to quickly run the SQL queries.

Further, I'll create another function that uses the run_query() function to return a list of all tables and views in the databases - this is handy to quickly check the state of the database during work.

As aforementioned, check the following output to see all tables which are included in the database:


```r
db <- 'chinook.db'
run_query <- function(q) {
  conn <- dbConnect(SQLite(), db)
  result <- dbGetQuery(conn, q)
  dbDisconnect(conn)
  return(result)
}

show_tables <- function() {
  q = "SELECT name, type FROM sqlite_master WHERE type IN ('table', 'view')"
  return(run_query(q))
}

# output existing tables inside chinook database:
show_tables()
```

```
##              name  type
## 1           album table
## 2          artist table
## 3        customer table
## 4        employee table
## 5           genre table
## 6         invoice table
## 7    invoice_line table
## 8      media_type table
## 9        playlist table
## 10 playlist_track table
## 11          track table
```

In this project, the received tables, were already checked on missing values, outliers and data types/classes. So I will not do these verifications as well as I will not do a inicial exploratory data analysis, which is normally done. Following I'll just answer some specific questions.

# Main Part - Answering Business Questions

### 1. Question - Selecting New Albums to Purchase

The Chinook record store has just signed a deal with a new record label, and you're in charge of choosing the first three albums to be added to the store. There are four albums to choose from, and all four are by artists who don't have any tracks in the store right now. Following is the list of artist names and the genre of music they produce:

Artist Name          | Genre
-------------------- | ------
Regal                | Hip-Hop
Red Tone             | Punk
Meteor and the Girls | Pop
Slim Jim Bites       | Blues

The record label specializes in artists from the USA, and they have given Chinook some money to advertise the new albums in the USA. To aid in selecting albums, the store is interested in finding out which genres sell the best in the USA. 

So the first question is: **Which music genre sells best in the USA? Make a recommendation for the three artists whose albums should be purchased for the store, based on the findings of best selling genre.**

To answer this question I'll need information from the tables:

- genre, to know the names of the existing genres
- track, table that joins information about the genre table and invoice_line table
- invoice_line, which tells me the quantity of how many times a track was bought
- invoice and customer, to know in which country the music was bought



```r
albums_to_purchase = '
                      WITH usa_tracks_sold AS
                         (
                          SELECT il.* FROM invoice_line AS il
                          INNER JOIN invoice AS i on il.invoice_id = i.invoice_id
                          INNER JOIN customer AS c on i.customer_id = c.customer_id
                          WHERE c.country = "USA"
                         )
                         
                      SELECT
                          g.name genre,
                          count(uts.invoice_line_id) tracks_sold,
                          cast(count(uts.invoice_line_id) AS FLOAT) / (
                              SELECT COUNT(*) from usa_tracks_sold
                          ) AS percentage_sold
                      FROM usa_tracks_sold AS uts
                      INNER JOIN track AS t on t.track_id = uts.track_id
                      INNER JOIN genre AS g on g.genre_id = t.genre_id
                      GROUP BY genre
                      ORDER BY tracks_sold DESC
                      LIMIT 10;
                      '
run_query(albums_to_purchase)
```

```
##                 genre tracks_sold percentage_sold
## 1                Rock         561      0.53377735
## 2  Alternative & Punk         130      0.12369172
## 3               Metal         124      0.11798287
## 4            R&B/Soul          53      0.05042816
## 5               Blues          36      0.03425309
## 6         Alternative          35      0.03330162
## 7                 Pop          22      0.02093245
## 8               Latin          22      0.02093245
## 9         Hip Hop/Rap          20      0.01902950
## 10               Jazz          14      0.01332065
```

Now that the result is available in a table format, I'll visualise the result into an interactive horizontal barplot using ggplot2:


```r
# save SQL result into R environment:
q1 <- run_query(albums_to_purchase)

# produce barplot:
q1v <- q1 %>%
  mutate(genre = fct_reorder(genre, tracks_sold)) %>%
  ggplot(aes(x = genre, y = tracks_sold)) +
    geom_col(fill="#f68060", alpha=.6, width=.6) +
    coord_flip() + 
    ggtitle("Most sold genres in the USA") +
    theme(plot.title = element_text(hjust = 0.5, size = 18)) +
    xlab("Genre") +
    ylab("Records Sold") 

ggplotly(q1v)
```

<!--html_preserve--><div id="htmlwidget-ec5c4dceb8d5fd03d10d" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-ec5c4dceb8d5fd03d10d">{"x":{"data":[{"orientation":"h","width":[0.6,0.6,0.6,0.6,0.6,0.6,0.6,0.600000000000001,0.600000000000001,0.600000000000001],"base":[0,0,0,0,0,0,0,0,0,0],"x":[14,20,22,22,35,36,53,124,130,561],"y":[1,2,3,4,5,6,7,8,9,10],"text":["genre: Jazz<br />tracks_sold:  14","genre: Hip Hop/Rap<br />tracks_sold:  20","genre: Latin<br />tracks_sold:  22","genre: Pop<br />tracks_sold:  22","genre: Alternative<br />tracks_sold:  35","genre: Blues<br />tracks_sold:  36","genre: R&B/Soul<br />tracks_sold:  53","genre: Metal<br />tracks_sold: 124","genre: Alternative & Punk<br />tracks_sold: 130","genre: Rock<br />tracks_sold: 561"],"type":"bar","marker":{"autocolorscale":false,"color":"rgba(246,128,96,0.6)","line":{"width":1.88976377952756,"color":"transparent"}},"showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":50.1386467413865,"r":7.30593607305936,"b":40.1826484018265,"l":130.776255707763},"plot_bgcolor":"rgba(235,235,235,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"title":{"text":"Most sold genres in the USA","font":{"color":"rgba(0,0,0,1)","family":"","size":23.9103362391034},"x":0.5,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-28.05,589.05],"tickmode":"array","ticktext":["0","200","400"],"tickvals":[0,200,400],"categoryorder":"array","categoryarray":["0","200","400"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":{"text":"Records Sold","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.4,10.6],"tickmode":"array","ticktext":["Jazz","Hip Hop/Rap","Latin","Pop","Alternative","Blues","R&B/Soul","Metal","Alternative & Punk","Rock"],"tickvals":[1,2,3,4,5,6,7,8,9,10],"categoryorder":"array","categoryarray":["Jazz","Hip Hop/Rap","Latin","Pop","Alternative","Blues","R&B/Soul","Metal","Alternative & Punk","Rock"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":{"text":"Genre","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":false,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","showSendToCloud":false},"source":"A","attrs":{"3fdc458f2802":{"x":{},"y":{},"type":"bar"}},"cur_data":"3fdc458f2802","visdat":{"3fdc458f2802":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->


As it can be seen from the graph above, Rock is by far the most sold genre in the USA, followed by Alternative & Punk and Metal. So, to answer the request:

- **Which music genre sells best in the USA?** And, make a recommendation for the three artists whose albums the store should purchase, based on the findings of best selling genre.

**Answer:** It can be said that the store should buy the albuns from the following three artists:

Artist Name          | Genre
-------------------- | ------
Red Tone             | Punk
Slim Jim Bites       | Blues
Meteor and the Girls | Pop


### 2. Question - Analyzing Employee Sales Performance

Each customer for the Chinook store gets assigned to a sales support agent within the company when they first make a purchase. Analyze the purchases of customers belonging to each employee to see who is the best sales support agent.

So the second question is: **Who is the best sales support agent**

To decide which Sales Agent is the best one, I will look at the total invoice value of each customer and after that group them per employee.


```r
successfull_empl <- '
                    WITH cust_total AS
                      (
                       SELECT c.*,
                              i.total AS sales_total
                       FROM customer AS c
                       INNER JOIN invoice AS i ON i.customer_id = c.customer_id
                      )
                    
                    SELECT  e.first_name || " " || e.last_name AS employee,
                            e.country,
                            SUM(ct.sales_total) AS sales
                    FROM employee AS e
                    INNER JOIN cust_total AS ct ON ct.support_rep_id = e.employee_id
                    GROUP BY e.employee_id
                    ORDER BY sales DESC;
                  '    

run_query(successfull_empl)
```

```
##        employee country   sales
## 1  Jane Peacock  Canada 1731.51
## 2 Margaret Park  Canada 1584.00
## 3 Steve Johnson  Canada 1393.92
```

**Answer:** As it can be seen, the **most successfull sales agent is Jane Peacock**. Further, the top 3 sales person are all from Canda.


### 3. Question - Customer Sales Performance per Country

Analyse the sales data for customers from each country. As we have a country variable in the invoice and in the customer table, I will only use the country value from the customers table, and ignore the country from the billing address in the invoice table.

Third question: **What are the sales performances in each country?**

In particular, calculate data, for each country, on the:

- Total number of customers
- Total value of sales (revenue)
- Total orders
- Average revenue per customer
- Average order value


```r
cust_stats_country <- '
                        WITH cust_inv AS
                          (
                          SELECT  
                                  CASE
                                       WHEN (
                                             SELECT COUNT(*)
                                             FROM customer
                                             WHERE country = c.country
                                            ) = 1 THEN "Other"
                                       ELSE c.country
                                   END AS country,                          
                                  c.*,
                                  SUM(i.total) AS sales,
                                  COUNT(DISTINCT i.invoice_id) AS num_orders
                          FROM customer AS c
                          INNER JOIN invoice AS i ON i.customer_id = c.customer_id
                          GROUP BY c.customer_id
                          )
                        
                        SELECT  country,
                                customers,
                                total_sales,
                                total_orders,
                                avg_rev_per_cust,
                                avg_order_value_per_cust
                        FROM
                              (SELECT
                                      ci.country,
                                      COUNT(*) AS customers,
                                      SUM(ci.sales) AS total_sales,
                                      SUM(ci.num_orders) AS total_orders,
                                      ROUND(AVG(sales), 2) AS avg_rev_per_cust,
                                      ROUND(SUM(ci.sales) / SUM(ci.num_orders), 2) AS avg_order_value_per_cust,
                                      CASE
                                          WHEN country = "Other"
                                              THEN 0
                                          ELSE 1
                                          END
                                      AS sort
                               FROM cust_inv AS ci
                               GROUP BY country
                               ORDER BY sort DESC, total_sales DESC
                              )
                      '

run_query(cust_stats_country)
```

```
##           country customers total_sales total_orders avg_rev_per_cust
## 1             USA        13     1040.49          131            80.04
## 2          Canada         8      535.59           76            66.95
## 3          Brazil         5      427.68           61            85.54
## 4          France         5      389.07           50            77.81
## 5         Germany         4      334.62           41            83.66
## 6  Czech Republic         2      273.24           30           136.62
## 7  United Kingdom         3      245.52           28            81.84
## 8        Portugal         2      185.13           29            92.57
## 9           India         2      183.15           21            91.58
## 10          Other        15     1094.94          147            73.00
##    avg_order_value_per_cust
## 1                      7.94
## 2                      7.05
## 3                      7.01
## 4                      7.78
## 5                      8.16
## 6                      9.11
## 7                      8.77
## 8                      6.38
## 9                      8.72
## 10                     7.45
```

As it can be seen above, the USA is the country where most of our customers come from and where we have in total numbers the highest performance. Next, for better understanding of the above result, I'll plot them into several graphs. As the created country "Other" is made out of several countries which only had 1 sale, I will exclude this one from the visualizations.



```r
# save table create previously via SQL query into the R environment
q3 <- run_query(cust_stats_country)

# exclude observation where country equal "Other"
q3_b <- q3 %>%
  filter(country != "Other")

# create ineractive barplot for total sales and orders:
p1 <-  
  plot_ly() %>%
    add_bars(x = ~q3_b$country, y = ~q3_b$total_sales, name = "Total Sales", alpha = 1,
             offsetgroup = 1) %>%
    add_bars(x = ~q3_b$country, y = ~q3_b$total_orders, name = "Total Orders", yaxis = "y2", alpha = 1, 
             offsetgroup = 2) %>%
    layout(
      title = "Total Sales & Orders",
      yaxis2 = list(overlaying = "y",
                    side = "right",
                    title = "Total Orders"),
      yaxis = list(title = "Total Sales"),
      xaxis = list(title = ""),
      margin = list(b = 10),
      barmode = "group"
    )

# create ineractive barplot for average customer revenue per customer and average order value per customer:
p2 <-
  plot_ly() %>%
    add_bars(x = ~q3_b$country, y = ~q3_b$avg_rev_per_cust, name = "avg_rev_per_cust", alpha = 1,
             offsetgroup = 1) %>%
    add_bars(x = ~q3_b$country, y = ~q3_b$avg_order_value_per_cust, name = "avg_order_value_per_cust", yaxis = "y2", alpha = 1, 
             offsetgroup = 2) %>%
    layout(
      title = "Average Customer Revenue & Order Value",
      yaxis2 = list(overlaying = "y",
                    side = "right",
                    title = "average order value per customer"),
      yaxis = list(title = "average revenue per customer"),
      xaxis = list(title = ""),
      margin = list(b = 10),
      barmode = "group"
    )  

p1
```

<!--html_preserve--><div id="htmlwidget-819d4f725184395d2214" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-819d4f725184395d2214">{"x":{"visdat":{"3fdc222a63c1":["function () ","plotlyVisDat"]},"cur_data":"3fdc222a63c1","attrs":{"3fdc222a63c1":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":{},"y":{},"type":"bar","name":"Total Sales","alpha":1,"offsetgroup":1,"inherit":true},"3fdc222a63c1.1":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":{},"y":{},"type":"bar","name":"Total Orders","yaxis":"y2","alpha":1,"offsetgroup":2,"inherit":true}},"layout":{"margin":{"b":10,"l":60,"t":25,"r":10},"title":"Total Sales & Orders","yaxis2":{"overlaying":"y","side":"right","title":"Total Orders"},"yaxis":{"domain":[0,1],"automargin":true,"title":"Total Sales"},"xaxis":{"domain":[0,1],"automargin":true,"title":"","type":"category","categoryorder":"array","categoryarray":["Brazil","Canada","Czech Republic","France","Germany","India","Portugal","United Kingdom","USA"]},"barmode":"group","hovermode":"closest","showlegend":true},"source":"A","config":{"showSendToCloud":false},"data":[{"x":["USA","Canada","Brazil","France","Germany","Czech Republic","United Kingdom","Portugal","India"],"y":[1040.49,535.59,427.68,389.07,334.62,273.24,245.52,185.13,183.15],"type":"bar","name":"Total Sales","offsetgroup":1,"marker":{"color":"rgba(31,119,180,1)","line":{"color":"rgba(31,119,180,1)"}},"error_y":{"color":"rgba(31,119,180,1)"},"error_x":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["USA","Canada","Brazil","France","Germany","Czech Republic","United Kingdom","Portugal","India"],"y":[131,76,61,50,41,30,28,29,21],"type":"bar","name":"Total Orders","yaxis":"y2","offsetgroup":2,"marker":{"color":"rgba(255,127,14,1)","line":{"color":"rgba(255,127,14,1)"}},"error_y":{"color":"rgba(255,127,14,1)"},"error_x":{"color":"rgba(255,127,14,1)"},"xaxis":"x","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

```r
p2
```

<!--html_preserve--><div id="htmlwidget-4cb2080d1eb819725af7" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-4cb2080d1eb819725af7">{"x":{"visdat":{"3fdc2a693c03":["function () ","plotlyVisDat"]},"cur_data":"3fdc2a693c03","attrs":{"3fdc2a693c03":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":{},"y":{},"type":"bar","name":"avg_rev_per_cust","alpha":1,"offsetgroup":1,"inherit":true},"3fdc2a693c03.1":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":{},"y":{},"type":"bar","name":"avg_order_value_per_cust","yaxis":"y2","alpha":1,"offsetgroup":2,"inherit":true}},"layout":{"margin":{"b":10,"l":60,"t":25,"r":10},"title":"Average Customer Revenue & Order Value","yaxis2":{"overlaying":"y","side":"right","title":"average order value per customer"},"yaxis":{"domain":[0,1],"automargin":true,"title":"average revenue per customer"},"xaxis":{"domain":[0,1],"automargin":true,"title":"","type":"category","categoryorder":"array","categoryarray":["Brazil","Canada","Czech Republic","France","Germany","India","Portugal","United Kingdom","USA"]},"barmode":"group","hovermode":"closest","showlegend":true},"source":"A","config":{"showSendToCloud":false},"data":[{"x":["USA","Canada","Brazil","France","Germany","Czech Republic","United Kingdom","Portugal","India"],"y":[80.04,66.95,85.54,77.81,83.66,136.62,81.84,92.57,91.58],"type":"bar","name":"avg_rev_per_cust","offsetgroup":1,"marker":{"color":"rgba(31,119,180,1)","line":{"color":"rgba(31,119,180,1)"}},"error_y":{"color":"rgba(31,119,180,1)"},"error_x":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["USA","Canada","Brazil","France","Germany","Czech Republic","United Kingdom","Portugal","India"],"y":[7.94,7.05,7.01,7.78,8.16,9.11,8.77,6.38,8.72],"type":"bar","name":"avg_order_value_per_cust","yaxis":"y2","offsetgroup":2,"marker":{"color":"rgba(255,127,14,1)","line":{"color":"rgba(255,127,14,1)"}},"error_y":{"color":"rgba(255,127,14,1)"},"error_x":{"color":"rgba(255,127,14,1)"},"xaxis":"x","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

**Answer:** Beside the observation already done previously to the total values, it is visible in the second graph with the average values, that **Czech Republic seems to have valuable customers, since their average revenue per customer as well as the order value per customer are the highest overall**. This might be an interesting country to increase future sales.

### 4. Question - Albums vs Individual Tracks

The Chinook store is set up in a way that allows customers to make purchases in one of two ways:

- Purchase a whole album
- Purchase a collection of one or more individual tracks

The store does not let customers purchase a whole album, and then add individual tracks to that same purchase (unless they do that by choosing each track manually). When customers purchase albums, they are charged the same price as if they had purchased each of those tracks separately.

Management are currently considering changing their purchasing strategy to save money. The strategy they are considering is to purchase only the most popular tracks from each album from record companies, instead of purchasing every track from an album.

We have been asked to find out what percentage of purchases are individual tracks versus whole albums. Management can use this data to understand the effect this decision might have on overall revenue.

Fourth question: **From all invoices (purchases), how many include complete album purchases and how many only some tracks and not the whole album?**

To do this analysis I'll need to categorize each invoice as either an album purchase or not, and than calculate summary statistics above the new categorical variable created and if they should continue to buy full albums from record companies.


```r
albums_vs_tracks <- ('
                    WITH invoice_first_track AS
                        (
                         SELECT
                             il.invoice_id AS invoice_id,
                             MIN(il.track_id) AS first_track_id
                         FROM invoice_line AS il
                         GROUP BY 1
                        )
                    SELECT
                        album_purchase,
                        COUNT(invoice_id) AS number_of_invoices,
                        CAST(count(invoice_id) AS FLOAT) / (
                                                             SELECT COUNT(*) FROM invoice
                                                          ) AS percent
                    FROM
                        (
                        SELECT
                            ifs.*,
                            CASE
                                WHEN
                                     (
                                      SELECT t.track_id FROM track AS t
                                      WHERE t.album_id = (
                                                          SELECT t2.album_id FROM track AS t2
                                                          WHERE t2.track_id = ifs.first_track_id
                                                         ) 
                                      EXCEPT 
                                      SELECT il2.track_id FROM invoice_line AS il2
                                      WHERE il2.invoice_id = ifs.invoice_id
                                     ) IS NULL
                                 AND
                                     (
                                      SELECT il2.track_id FROM invoice_line AS il2
                                      WHERE il2.invoice_id = ifs.invoice_id
                                      EXCEPT 
                                      SELECT t.track_id FROM track AS t
                                      WHERE t.album_id = (
                                                          SELECT t2.album_id FROM track AS t2
                                                          WHERE t2.track_id = ifs.first_track_id
                                                         ) 
                                     ) IS NULL
                                 THEN "yes"
                                 ELSE "no"
                             END AS "album_purchase"
                         FROM invoice_first_track AS ifs
                        )
                    GROUP BY album_purchase;
                  ')

run_query(albums_vs_tracks)
```

```
##   album_purchase number_of_invoices   percent
## 1             no                500 0.8143322
## 2            yes                114 0.1856678
```

**Answer:** From the table above it can be seen that in most of the purchases, the customer do not by the whole album but rather only specific tracks they are interest. So, it can be recommended to the management, that they should stop buying full albums from record companies and start buying only single tracks.

# Conclusion

In this challenging project, provided by [dataquest.io](https://www.dataquest.io/), SQL skills like advanced joins, subqueries, multiple joins, set operations and aggregate functions were used to get the necessay data to answer the questions.  Support sources like [stackoverflow](https://stackoverflow.com/), the [r-graph-gallery](https://www.r-graph-gallery.com/index.html) or [data.world](https://docs.data.world/documentation/sql/concepts/intermediate/intermediate_intro.html) were used to clear doubts regarding SQL or R syntaxes and concepts.

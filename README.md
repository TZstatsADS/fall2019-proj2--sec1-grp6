# Project 2: Shiny App Development Version 2.0

### [Project Description](doc/project2_desc.md)

![screenshot](doc/Restaurant.png)

In this second project of GR5243 Applied Data Science, we develop a version 2.0 of an *Exploratory Data Analysis and Visualization* shiny app on a topic of your choice using [NYC Open Data](https://opendata.cityofnewyork.us/) or U.S. government open data released on the [data.gov](https://data.gov/) website. See [Project 2 Description](doc/project2_desc.md) for more details.  

The **learning goals** for this project is:

- business intelligence for data science
- study legacy codes and further development
- data cleaning
- data visualization
- systems development/design life cycle
- shiny app/shiny server

*The above general statement about project 2 can be removed once you are finished with your project. It is optional.

## Tasty and Safety
Term: Fall 2019

+ Team 6
+ **Projec title**: Tasty and Safety: Restaurant Safety Information App 
+ **Team members**:
	+ Chen, Luyue
	+ He, Chongyu
	+ Kravitz, Adam
	+ Wu, Wenyue
	+ Zhuo, Na

+ **Project link**: https://lovelydoggy.shinyapps.io/Final_presentation/  
+ **Project summary**: The App is a tool to connect users to New York City restaurants' food safety inpsection results. In this app, users could explore the food-safety landscape among different boroughs, cuisines, and neighborhoods from various angles such as violations, scores, and grades. The whole app contains three overview tabs: Map Overview, Comparison and Restuarant Lookup.    

	+ Map Overview  
	The first part gives users a customizable heatmap with an overview of average inspection score based on selected filters.
        
	+ Comparation  
	This part allows a more nuanced comparison between selections based on various filters. "Top vialotion" shows most-frequent violations of restaurants. "Inspection scores" and "Inspection grades" shows the breakdowns of scores and grades restaurants received at the most recent inspection. 

	+ Restuarant Lookup  
	The final part "Restaurant Lookup" would allow users to lookup restaurants according to cusine type, borough and grades. Users can click on search results to identify their restaurants on the map and see further detailed information on the "Detail" tab. 

+ **Contribution statement**:All team members contributed to the completion of this project. All team members approve our work presented in this GitHub repository including this contributions statement. 

Data acquisition and manipulation: Everyone

UI Design: Wenyue Wu, Adam Kravitz, Na Zhuo, Luyue Chen

  Server-Comparison Page: Wenyue Wu
  
  Server-Heatmap Page: Adam Kravitz,Wenyue Wu
  
  Server-Recommendation Search page: Na Zhuo (mostly) Luyue Chen
  
  Server-Recommendation Detail page: Wenyue Wu
  
  Server-contact page: Luyue Chen (mostly) Na Zhuo
  
Code Intergration: Wenyue Wu, Adam Kravitz

Debug and Finalize: Wenyue Wu

Presentation: Chongyu He

Github arrangement: Chongyu He 


Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.

```
proj/
├── app/
├── lib/
├── data/
├── doc/
└── output/
```




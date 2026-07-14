
🔗[[Link Text](https://docs.google.com/document/d/1BQ2aXXNXumzOKh88ExKNtKRcnrIsVSJyydotG2LBJUY/edit?tab=t.0)] 
INTRODUCTION:
This paper explores the dangers of an uneducated society and how those from lower-income communities are disproportionately affected by it based on race, gender, and disabilities. Over the last few years, there has been an outpouring of educators on social media platforms that have time and time again, stated that they are quitting their jobs in droves because of how disruptive and unknowledgeable they are. Libraries are being shut down, and books are being banned throughout the country. Social media and the advances of technology (AI) have a huge impact as and are inhibiting young people's ability to think critically. The sheer amount of ignorance and misinformation that is being shared and documented in news outlets by our government and politicians just validate the decline of critical thinking.
______________________________________________________________________________________________________________________________
RESEARCH QUESTION:

Is anti-intellectualism rhetoric happening in schools in New York? It is well established that in poorer neighborhoods public schools are well understaffed, underfunded, and attendance rates are at an all time high.

______________________________________________________________________________________________________________________________
METHODS: DATA SOURCES

This analysis utilized data from the U.S. Census (2022) to explore how anti-intellectualism may be influencing educational outcomes among students in New York. Key variables included race (Black and White populations), high school graduation status (graduates and non-graduates), educational attainment (some college or associate degree, and bachelor’s degree), and median age. All variables were measured across the total population, inclusive of all genders. Initially, a path analysis was considered as the most appropriate analytical framework due to the assumed interrelatedness of variables. However, after further evaluation, an Ordinary Least Squares (OLS) regression model was determined to be more suitable for this dataset and research question. The shift to regression analysis allowed for a clearer interpretation of how these demographic factors relate to educational attainment, particularly in the context of anti-intellectual attitudes.
______________________________________________________________________________________________________________________________
RESULTS AND DISCUSSION:

Using the census I conducted, at first, correlation charts, and then several models for my path analysis. But it seemed like every model I fit, the data became saturated, and did not behave in theory as expected. So, dropping that, I assumed the OLS regression model on whether or not county-level population size predicts the proportion with no high school graduation. 

FIGURE 1.
<img width="440" height="226" alt="Rplot03" src="https://github.com/user-attachments/assets/0df43fe1-294f-42ec-8af1-0257d46c4ff0" />

This graph displays that there is no evidence that the county population size predicts the proportion of people with no high school graduation in the data. Another multiple regression model was created, this time with variables of total_race, and white, were included. This model displays that for each additional person in the county, the proportion with no high school graduation decreases by a small amount. So larger counties tend to have a slightly lower proportion of people with no high school graduation, holding the variable (-0.1) prop_white constant. For each -1 unit increases in the proportion of white residents, the proportion with no high school graduation decreases by 0.14, holding the variable total_race constant. Meaning, counties with a higher proportion of white residents have a lower proportion with no highschool graduation. 


FIGURE 2.
<img width="440" height="226" alt="Rplot04" src="https://github.com/user-attachments/assets/f48dfbad-10ab-4027-86ec-ff087d3b24c7" />





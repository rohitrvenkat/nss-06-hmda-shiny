# Data Question: R Shiny Dashboard (Hauser Jones & Sas)

Hauser Jones & Sas is an accounting firm with specializes in the audit of banks, credit unions and other financial institutions. 

In this project, you will be working toward creating a dashboarding tool that will aid in their audit function.

The deliverable for this project is an app built with R Shiny that can be used to help auditors understand and derive insights on how a loan institution's lending practices compare to the peer institutions within their geographic region. This will help these institutions identify insuffiencies in current lending operations and ensure compliance to regulatory guidelines specified through the Equal Credit Opportunity Act (ECOA).

### **Part 1:** 
Start by exploring what current data is available through the [The Home Mortgage Disclosure Act (HMDA)](https://ffiec.cfpb.gov/) website. Data can be manually pulled using the dataset filtering browser [(Link)](https://ffiec.cfpb.gov/data-browser/data/2020?category=nationwide). Additionally, you might consider using the [*HMDA API*](https://cfpb.github.io/hmda-platform/#hmda-api-documentation) to help retrieve the needed data in a usable format rather than manually downloading the data directly from the website. For the minimum viable product (MVP), you should focus on finding data that can be used to understand the demographic makeup of a region, including ethnicity/race, gender, age, and disability. Further developments of the application can include features such as familial status, approved loans, loan type, dwelling category, income.


### **Part 2:** 
Build an app using R Shiny. This app should allow the user view loan applications data by selectable institutions, geographic areas and filter by gender/race/age/disability.

## **Minimum Requirements**
  - Filter by geographical area (County, State, or MSA/Census Tract)
  - Filter by lender (Legal Entity Identifier or LEI)
  - Table and visual showing select LEI compared to selected geographical area (County, State, or MSA/Census Tract)
  - One or more pages to where you can select a lending institution by its LEI, and be able to select a subset of peer institutions based on geographical area (County, State, MSA/Census Tract) to show what percentage and # of loans were applied for by the following:
    - Race
    - Gender
    - Age
    - Disability
  

---

### **Bonus Features**
 - Filter by loan type
 - Filter by type of purchaser
 - Filter by dwelling category/type
 - Distribution by loan amounts (0-50,50-100,100-150,200+ etc by 100k) (Histograph or Box chart)
 - One page to display # and percentage loans appliications by county in map form
 - Visual/Table on Application Credit Scores
 - Visual/Table on denial reason proportions
 - Percentage split by Action Taken Feature
 - Filter by Year or select Multiyear


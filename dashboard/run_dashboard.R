---
title: "BAARD biotyping and data modelling"
author: "Amber Fedynak"
date: "`r Sys.Date()`"
output: 
  flexdashboard::flex_dashboard:
runtime: shiny
---

```{r global, include=FALSE}
library(dplyr)
library(stringr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(readxl)
library(plotly)
library(DT)


madrs_data <- read.csv("MADRS_Longitudinally_20230711.csv")
madrs_data <- as.data.frame(my_data)
madrs_data$site <- stringr::str_extract(my_data$ID, "^.{2}")
madrs_data <- my_data %>% filter(!grepl("CU", my_data$site))


df = data.frame(select(my_data, as.name("ID")), 
                select(my_data, as.name("TimePoint")),
                select(my_data, as.name("madrs_date")),
                check.names=FALSE)
df$madrs_date <- lubridate::mdy(df$madrs_date)

df_list <- list(
  All = df,
  LA = df %>% filter(str_detect(ID, '^LA')),
  UP = df %>% filter(str_detect(ID, '^UP')),
  UT = df %>% filter(str_detect(ID, '^UT')),
  WU = df %>% filter(str_detect(ID, '^WU'))
)

```
# Overview

## Column 1 {data-width=600}

### Study Overview {data-height=650} 
**Background**
Depression in older adults often proves challenging to treat, with up to 50% not responding to initial antidepressant therapy and fewer than 20% achieving remission. Utilizing a robust multi-site clinical trial network, we have conducted two landmark randomized controlled trials (RCTs) that demonstrated the efficacy of augmenting antidepressant treatment with aripiprazole (ARI) or bupropion (BUP). These interventions achieved a 29% remission rate in treatment-resistant late-life depression (TRLLD).
**Objective**
Building on the findings of the OPTNEURO and OPT studies, we aim to advance the BAARD (Biotype-assigned Augmentation Approach in Resistant Late-Life Depression) study. Our primary objective is to improve treatment selection for late-life depression (LLD) by leveraging precision biomedical information.
**Data Integration and Analysis**
The BAARD initiative seeks to develop a clinical decision support tool to enhance treatment selection for TRLLD. This effort integrates diverse expertise, including geroscience, psychopharmacology, cognition, molecular subtyping, neuroimaging, computational psychiatry, and qualitative research methodologies.
**Significance**
To achieve these goals, we will utilize comprehensive demographic, clinical, cognitive, genetic, proteomic, and neuroimaging data from approximately 700 participants. The development and testing of the BAARD tool aim to significantly improve remission rates and transform care delivery for this vulnerable population.



### {data-height=350}
```{r}
df_p <- data.frame(c("University of Pittsburgh School of Medicine (UPSOM)", "Centre for Addiction and Mental Health (CAMH)", "University of California - Los Angeles (UCLA)", "Washington University School of Medicine (WUSM)", "Total"),
  c(124, 85, 91, 87, 397))
colnames(df_p) <- c("Institution", "No. Patients")
knitr::kable(df_p)

```

## Row 2 {data-width=400}


### Data modelling {data-height=750}
```{r}
renderImage({
      list(src = "/Users/afedynak/dashboard/data/workflow.jpeg", contentType = "image/jpg", width="420px", height="460px", align="fill")
})
```
### {data-height=75}

```{r}
renderImage({
      list(src = "/Users/afedynak/dashboard/data/kcni_logo.jpeg", contentType = "image/jpg", width="250px", height="50px", align="right")
})
```

# Summary

## sidebar {.sidebar}
```{r}
selectInput("dataset",
  label = strong("Site"),
  choices = c("All", "LA", "UP", "UT", "WU"),
  selected = "All"
)
```

## Column 1 {data-orientation=rows}

### Number of entries (MADRS only) {data-height=750}
```{r}
count_site <- my_data %>% group_by(site, TimePoint) %>% summarize( my_count = n())
count_all <- my_data %>% group_by(TimePoint) %>% summarize(my_count=n())
count_all$site <- "All"
my_totals <- rbind(count_all, count_site)

#renderPlot({ggplot(my_totals, aes(x=TimePoint, y=my_count, group=site, color=site)) #+
renderPlot({
  my_totals <- my_totals %>% filter(site == input$dataset)
  
  ggplot(my_totals, aes(x=TimePoint, y=my_count, group=site, color=site)) +
  geom_line(size=2) +
  ylab("Total number") +
  xlab("Timepoint") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle=45, hjust=1, size=12), 
    axis.text.y = element_text(size=12),
    axis.title.x = element_text(size=12),
    axis.title.y = element_text(size=12),
    legend.text = element_text(size=11),
    panel.grid.minor.x = element_blank())
})
```
### {data-height=350}


## {data-width=200}
### Timepoints {data-width=200}
#### Step 1
* Baseline
* Week 0 (Wk0)
* Week 10 (Wk10)
* Month 4 (M4)
* Month 8 (M8)
* Month 12 (M12)

#### Step 2
* Baseline
* Week 0 (Wk0)
* Week 10 (Wk10)
* Month 4 (M4)
* Month 8 (M8)
* Month 12 (M12)
* Month 24 (M24 Neuro)

# Timeline

## Column 1 {data-width=650, .tabset}

### LA Dataset

```{r}
      myplot <- ggplot(data=df_list$LA, aes(x=madrs_date, y=ID, colour=TimePoint)) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
      geom_point(size=2) + theme_minimal() +
      theme(axis.text.x = element_text(angle=45, hjust=1, size=6), 
            axis.text.y = element_blank()) +
      xlab("Date") +
      ylab("Patient ID")
      myplot %>% plotly::ggplotly()
```
### UP Dataset

```{r}
      myplot <- ggplot(data=df_list$UP, aes(x=madrs_date, y=ID, colour=TimePoint, height=10)) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
      geom_point(size=2) + theme_minimal() +
      theme(axis.text.x = element_text(angle=45, hjust=1, size=6), 
            axis.text.y = element_blank()) +
      xlab("Date") +
      ylab("Patient ID")
      myplot %>% plotly::ggplotly()
```
### UT Dataset

```{r}
      myplot <- ggplot(data=df_list$UT, aes(x=madrs_date, y=ID, colour=TimePoint, height=10)) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
      geom_point(size=2) + theme_minimal() +
      theme(axis.text.x = element_text(angle=45, hjust=1, size=6), 
            axis.text.y = element_blank()) +
      xlab("Date") +
      ylab("Patient ID")
      myplot %>% plotly::ggplotly()
```
### WU Dataset

```{r}
      myplot <- ggplot(data=df_list$WU, aes(x=madrs_date, y=ID, colour=TimePoint, height=10)) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
      geom_point(size=2) + theme_minimal() +
      theme(axis.text.x = element_text(angle=45, hjust=1, size=6), 
            axis.text.y = element_blank()) +
      xlab("Date") +
      ylab("Patient ID")
      myplot %>% plotly::ggplotly()
```

# MADRS

## Column 1 {data-width=900, .tabset}

### MADRS (Normalized)
```{r}
df <- read.csv("/Users/afedynak/dashboard/data/MADRS_Longitudinally_20230711.csv")
df$madrs_date <- lubridate::mdy(df$madrs_date)
df1 <- df %>% filter(grepl('^01_Step1Baseline', df$TimePoint)) 
df2 <- df %>% filter(grepl('^03_Step1Wk10', df$TimePoint)) 
df3 <- df1 %>% inner_join(df2, by = "ID")
df3 <- df3 %>% distinct() %>%
  mutate(Total_Days = as.numeric(difftime(ymd(Date2),  ymd(Date1), units = "days")))
colnames(df3) <- c("PatientID", "Timepoint1", "Date1", "Score1", "Timepoint2", "Date2", "Score2", "Site")

renderPlot({ggplot(data=df3, aes(x=Total_Days, y=PatientID)) +
  geom_point(shape = 21, size = 5, alpha = 8/10) +
  aes(fill = Timepoint1) +
  theme_bw() +
  xlab("Number of days") +
  ylab("Patient ID") +
  scale_x_continuous(breaks = seq(-100,400,100), lim = c(-125,450)) +
  scale_fill_discrete(name = "", labels = c("Baseline", "Week 0")) +
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold"),
        legend.text = element_text(size=14))
})

```



## Column 3 {data-width=250}

### Days from baseline {data-width=900}
```{r}
df_summary = data.frame(select(df3, as.name("Total_Days")))

colnames(df3) <- c("Number of days")
renderPrint({summary(df3)})
```

## Column 4 {data-width=50}


# Medication
## Column 1{data-width=100}
## Column 2{data-width=600}
### {data-height=800}
```{r}
df <- read.csv("/Users/afedynak/dashboard/data/medication_AF.csv")

df$bup_flag <- apply(df, 1, function(row) {
  if (any(str_detect(tolower(row), "bupropion"))) {
    return("1")
  } else {
    return(NA)
  }
})

df$ari_flag <- apply(df, 1, function(row) {
  if (any(str_detect(tolower(row), "aripiprazole"))) {
    return("1")
  } else {
    return(NA)
  }
})

df$adh_flag <- apply(df, 1, function(row) {
  if (any(str_detect(tolower(row), "not taking it at all"), na.rm = TRUE)) {
    return("1")
  } else {
    return(NA)
  }
})

df$both_flagged <- ifelse(!is.na(df$bup_flag) & !is.na(df$ari_flag), "1", NA)
both_flagged <- df[!is.na(df$both_flagged), ]
df_ari <- df[!is.na(df$ari_flag) & is.na(df$bup_flag), ]
df_bup <- df[!is.na(df$bup_flag) & is.na(df$ari_flag), ]

df_meds <- data.frame (
  Treatment = c("Bupropion", "Aripiprazole", "Bupropion + Aripiprazole"),
  Total_count = c(nrow(df_bup), nrow(df_ari), nrow(both_flagged))
)

renderPlot({ggplot(df_meds, aes(x=Treatment, y=Total_count)) +
  geom_col(fill="grey70", colour="black", width=0.6) +
  geom_text(aes(label=Total_count), vjust=-0.3, size=6, face="bold") +
  #theme_minimal() +
  ylab("Total Number") +
  #xlab("Medication") +
  theme_minimal() +
  #theme(axis.text.x = element_text(size=12, face="bold"),
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=14))
})

```
### {data-height=200}
- Treatment with Aripiprazole + Bupropion
- Participants with "Other" treatment

## Column 3{data-width=100}


# Blood biomarkers
## Column 2{data-width=900}
### {data-height=900}
```{r}
df <- read.csv("/Users/afedynak/dashboard/data/O-Neuro_SASP_Sep2023.csv")

df_long <- df %>%
  select(-c("Total.Number.of.samples", "Identification.1st.Round", "Identification.Erica", "X", "SASP")) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

#renderImage({
renderPlot({
  ggplot(df_long, aes(x = 1:nrow(df_long), y = Value)) +
  geom_violin(trim=FALSE, fill="lightgrey") +
  facet_wrap(~ Variable, ncol=6, scales = "free_y") + 
  theme_minimal() +
  labs(y="ug/ml") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(),
        axis.text.x = element_blank(),
        strip.text = element_text(size = 12),
        plot.title = element_text(face = "bold"))
})
```
### {data-height=100}
- 22 markers for OPTNEURO; Expanded panel for BAARD
- Data cleaning in-progress


# Demographic

## Column 1 {data-width=500}

### {data-height=800}
```{r}
df <- read_excel("/Users/afedynak/dashboard/data/baseline_s1_cleaning_v3.0_per_protocol.xlsx", sheet = "edited")
df$site <- stringr::str_extract(df$ID, "^.{2}")
df_filtered <- df %>% filter(!grepl("CU", df$site))

renderPlot({ggplot(mapping = aes(x = age, fill = Gender)) +
  geom_histogram(data = df_filtered %>% filter(Gender == "Female"),
                 breaks = seq(55, 100, 5),
                 fill = "#7665a8",
                 colour = "white") +
  geom_histogram(data = df_filtered %>% filter(Gender == "Male"),
                 breaks = seq(55, 100, 5),
                 mapping = aes(y = ..count..*(-1)),
                 fill = "#ebb367",
                 colour = "white") +
  coord_flip() +
  labs(x = "Age",
       y = "Frequency",
       fill = "Gender") +
      scale_fill_manual(values = c("Female" = "#7665a8", "Male" = "#b4721b")) +  # Manual color scale for legend clarity
  theme_minimal() +
    theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size=12),
        legend.title = element_text(size=14),
        legend.position = "right",  # Move the legend outside the plot
        legend.box = "vertical",
        legend.margin = margin(0, 10, 0, 0))
})
```

### {data-height=200}
```{r}
renderImage({
      list(src = "/Users/afedynak/dashboard/data/legend.jpg", contentType = "image/jpg", width="400px", height="80px", align="fill")
})
```

## Column 3 {data-width=500}
### Ethnicity
```{r}

renderPlot({ggplot(df_filtered, aes(x = reorder(Race, Race, FUN = length))) +
  geom_histogram(stat="count", fill = "#77bab9") +
  ylab("Total number") +
  xlab("Race") +
  theme_minimal() +
theme(axis.text.x = element_text(angle=60, hjust=1, size=14), 
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 14))
})
```


# Table

## Column 2 {data-width=1000}

###
```{r}
my_df <- read.csv("/Users/afedynak/dashboard/data/OPTN_Data_Request_9_BAARD_20240127.csv")

DT::renderDataTable(my_df, 
              rownames = FALSE, 
              options = list(pageLength = 50, scrollY = TRUE), 
              class = 'white-space: nowrap',
              )
```


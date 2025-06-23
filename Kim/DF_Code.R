library(tidyverse)
library(factoextra)
library(ggplot2)
library(ggmosaic)
df <- read.csv("analytic_data2025_v2.csv")
view(df)
names(df)
df <- df %>% 
  select(Poor.Mental.Health.Days.raw.value, Frequent.Mental.Distress.raw.value, 
         Feelings.of.Loneliness.raw.value, Poor.Physical.Health.Days.raw.value, 
         Frequent.Physical.Distress.raw.value, X..Below.18.Years.of.Age.raw.value, 
         X..65.and.Older.raw.value, X..Female.raw.value, X..Hispanic.raw.value, 
         X..Non.Hispanic.Black.raw.value, X..Non.Hispanic.White.raw.value, 
         X..American.Indian.or.Alaska.Native.raw.value, X..Asian.raw.value, 
         X..Native.Hawaiian.or.Other.Pacific.Islander.raw.value, 
         High.School.Graduation.raw.value, Some.College.raw.value, 
         High.School.Completion.raw.value, Unemployment.raw.value, 
         Children.in.Poverty.raw.value, Income.Inequality.raw.value, 
         Severe.Housing.Problems.raw.value, Severe.Housing.Cost.Burden.raw.value, 
         Homeownership.raw.value, Food.Insecurity.raw.value, Living.Wage.raw.value, 
         Child.Care.Cost.Burden.raw.value, Children.Eligible.for.Free.or.Reduced.Price.Lunch.raw.value, 
         Uninsured.raw.value, Uninsured.Adults.raw.value, Uninsured.Children.raw.value, 
         Ratio.of.population.to.primary.care.physicians., 
         Ratio.of.population.to.mental.health.providers., Mental.Health.Providers.raw.value, 
         Primary.Care.Physicians.raw.value, Other.Primary.Care.Providers.raw.value, 
         Children.in.Single.Parent.Households.raw.value, Social.Associations.raw.value, 
         Disconnected.Youth.raw.value, Lack.of.Social.and.Emotional.Support.raw.value, 
         Adult.Smoking.raw.value, Excessive.Drinking.raw.value, Physical.Inactivity.raw.value, 
         Insufficient.Sleep.raw.value, Sexually.Transmitted.Infections.raw.value, 
         Teen.Births.raw.value, HIV.Prevalence.raw.value, Adult.Obesity.raw.value, 
         Diabetes.Prevalence.raw.value, Air.Pollution..Particulate.Matter.raw.value, 
         Adverse.Climate.Events.raw.value, Residential.Segregation...Black.White.raw.value, 
         Limited.Access.to.Healthy.Foods.raw.value, Access.to.Exercise.Opportunities.raw.value, 
         Drug.Overdose.Deaths.raw.value, Alcohol.Impaired.Driving.Deaths.raw.value, 
         Injury.Deaths.raw.value, Suicides.raw.value, Crude.suicide.rate)

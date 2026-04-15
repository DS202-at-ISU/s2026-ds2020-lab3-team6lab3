
<!-- README.md is generated from README.Rmd. Please edit the README.Rmd file -->

# Lab report \#3 - instructions

Follow the instructions posted at
<https://ds202-at-isu.github.io/labs.html> for the lab assignment. The
work is meant to be finished during the lab time, but you have time
until Monday evening to polish things.

Include your answers in this document (Rmd file). Make sure that it
knits properly (into the md file). Upload both the Rmd and the md file
to your repository.

All submissions to the github repo will be automatically uploaded for
grading once the due date is passed. Submit a link to your repository on
Canvas (only one submission per team) to signal to the instructors that
you are done with your submission.

# Lab 3: Avenger’s Peril

## As a team

Extract from the data below two data sets in long form `deaths` and
`returns`

``` r
av <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/avengers/avengers.csv", stringsAsFactors = FALSE)
head(av)
```

    ##                                                       URL
    ## 1           http://marvel.wikia.com/Henry_Pym_(Earth-616)
    ## 2      http://marvel.wikia.com/Janet_van_Dyne_(Earth-616)
    ## 3       http://marvel.wikia.com/Anthony_Stark_(Earth-616)
    ## 4 http://marvel.wikia.com/Robert_Bruce_Banner_(Earth-616)
    ## 5        http://marvel.wikia.com/Thor_Odinson_(Earth-616)
    ## 6       http://marvel.wikia.com/Richard_Jones_(Earth-616)
    ##                    Name.Alias Appearances Current. Gender Probationary.Introl
    ## 1   Henry Jonathan "Hank" Pym        1269      YES   MALE                    
    ## 2              Janet van Dyne        1165      YES FEMALE                    
    ## 3 Anthony Edward "Tony" Stark        3068      YES   MALE                    
    ## 4         Robert Bruce Banner        2089      YES   MALE                    
    ## 5                Thor Odinson        2402      YES   MALE                    
    ## 6      Richard Milhouse Jones         612      YES   MALE                    
    ##   Full.Reserve.Avengers.Intro Year Years.since.joining Honorary Death1 Return1
    ## 1                      Sep-63 1963                  52     Full    YES      NO
    ## 2                      Sep-63 1963                  52     Full    YES     YES
    ## 3                      Sep-63 1963                  52     Full    YES     YES
    ## 4                      Sep-63 1963                  52     Full    YES     YES
    ## 5                      Sep-63 1963                  52     Full    YES     YES
    ## 6                      Sep-63 1963                  52 Honorary     NO        
    ##   Death2 Return2 Death3 Return3 Death4 Return4 Death5 Return5
    ## 1                                                            
    ## 2                                                            
    ## 3                                                            
    ## 4                                                            
    ## 5    YES      NO                                             
    ## 6                                                            
    ##                                                                                                                                                                              Notes
    ## 1                                                                                                                Merged with Ultron in Rage of Ultron Vol. 1. A funeral was held. 
    ## 2                                                                                                  Dies in Secret Invasion V1:I8. Actually was sent tto Microverse later recovered
    ## 3 Death: "Later while under the influence of Immortus Stark committed a number of horrible acts and was killed.'  This set up young Tony. Franklin Richards later brought him back
    ## 4                                                                               Dies in Ghosts of the Future arc. However "he had actually used a hidden Pantheon base to survive"
    ## 5                                                      Dies in Fear Itself brought back because that's kind of the whole point. Second death in Time Runs Out has not yet returned
    ## 6                                                                                                                                                                             <NA>

Get the data into a format where the five columns for Death\[1-5\] are
replaced by two columns: Time, and Death. Time should be a number
between 1 and 5 (look into the function `parse_number`); Death is a
categorical variables with values “yes”, “no” and ““. Call the resulting
data set `deaths`.

``` r
library(tidyverse)
deaths <- av %>%
  pivot_longer(
    cols = c(Death1, Death2, Death3, Death4, Death5),
    names_to = "Time",
    values_to = "Death"
  ) %>%
  mutate(
    Time = parse_number(Time),
    Death = factor(str_to_lower(Death), levels = c("", "no", "yes"))
  )

returns <- av %>%
  pivot_longer(
    cols = c(Return1, Return2, Return3, Return4, Return5),
    names_to = "Time",
    values_to = "Return"
  ) %>%
  mutate(
    Time = parse_number(Time),
    Return = factor(str_to_lower(Return), levels = c("", "no", "yes"))
  )
```

Similarly, deal with the returns of characters.

Based on these datasets calculate the average number of deaths an
Avenger suffers.

``` r
avg_deaths <- av %>%
  distinct(Name.Alias) %>%
  left_join(
    deaths %>%
      filter(Death == "yes") %>%
      count(Name.Alias, name = "num_deaths"),
    by = "Name.Alias"
  ) %>%
  mutate(num_deaths = replace_na(num_deaths, 0)) %>%
  summarise(avg = mean(num_deaths))

avg_deaths
```

    ##         avg
    ## 1 0.5460123

## Individually

For each team member, copy this part of the report.

Each team member picks one of the statements in the FiveThirtyEight
[analysis](https://fivethirtyeight.com/features/avengers-death-comics-age-of-ultron/)
and fact checks it based on the data. Use dplyr functionality whenever
possible.

### FiveThirtyEight Statement

> Quote the statement you are planning to fact-check.

### Include the code

### Include your answer

Make sure to include the code to derive the (numeric) fact for the
statement

### Shiva

> Out of 173 listed Avengers, my analysis found that 69 had died at
> least one time after they joined the team.

``` r
deaths_summary <- av %>%
  mutate(
    died_at_least_once =
      Death1 == "YES" |
      Death2 == "YES" |
      Death3 == "YES" |
      Death4 == "YES" |
      Death5 == "YES"
  ) %>%
  summarise(
    total_avengers = n(),
    died_at_least_once = sum(died_at_least_once, na.rm = TRUE)
  )

deaths_summary
```

    ##   total_avengers died_at_least_once
    ## 1            173                 69

The analysis shows that 69 out of 173 Avengers have died at least once,
which matches the claim made in the article. This confirms that about
40% of Avengers experience death during their time on the team.

### Jensen

> I counted 89 total deaths — some unlucky Avengers7 are basically Meat
> Loaf with an E-ZPass — and on 57 occasions the individual made a
> comeback

``` r
deaths %>%
  filter(Death == "yes") %>%
  summarise(NumberofDeaths = sum(n())) 
```

    ## # A tibble: 1 × 1
    ##   NumberofDeaths
    ##            <int>
    ## 1             89

``` r
returns %>%
  filter(Return == "yes") %>%
  summarise(NumberofReturns = sum(n())) 
```

    ## # A tibble: 1 × 1
    ##   NumberofReturns
    ##             <int>
    ## 1              57

We can see a total number of 89 deaths from the deaths set and a total
number of 57 returns from the return dataset, meaning his statement is
true.

### Pablo

> What’s more, if you’re a fan of the MCU, nobody is safe. Of the nine
> Avengers we see on screen — Iron Man, Hulk, Captain America, Thor,
> Hawkeye, Black Widow, Scarlet Witch, Quicksilver and The Vision —
> every single one of them has died at least once in the course of their
> time Avenging in the comics. In fact, Hawkeye died twice!6

``` r
deaths %>%
  distinct(Name.Alias) %>%
  pull(Name.Alias) %>%
  grep(pattern = "Natalia", value = TRUE)
```

    ## [1] "Natalia Alianovna Romanova"

``` r
deaths |>
  select(Name.Alias, Honorary, Time, Death) |>
  filter(Name.Alias %in% c(
    'Anthony Edward "Tony" Stark',
    "Robert Bruce Banner",
    "Wanda Maximoff",
    "Thor Odinson",
    "Steven Rogers",
    "Pietro Maximoff",
    "Victor Shade (alias)",
    "Clinton Francis Barton",
    "Natalia Alianovna Romanova"
  )) |>
  filter(Death == "yes", !is.na(Time)) |>
  group_by(Name.Alias) |>
  summarise(latest_death_time = max(Time), .groups = "drop") |>
  arrange(desc(latest_death_time))
```

    ## # A tibble: 9 × 2
    ##   Name.Alias                      latest_death_time
    ##   <chr>                                       <dbl>
    ## 1 "Clinton Francis Barton"                        2
    ## 2 "Thor Odinson"                                  2
    ## 3 "Anthony Edward \"Tony\" Stark"                 1
    ## 4 "Natalia Alianovna Romanova"                    1
    ## 5 "Pietro Maximoff"                               1
    ## 6 "Robert Bruce Banner"                           1
    ## 7 "Steven Rogers"                                 1
    ## 8 "Victor Shade (alias)"                          1
    ## 9 "Wanda Maximoff"                                1

All 9 Avengers listed do in fact die at least once. Hawkeye and Thor
both had latest death times of 2, so the data also supports the claim
that Hawkeye (Clint Barton) died twice.

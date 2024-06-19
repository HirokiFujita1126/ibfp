
# ibfp

<!-- badges: start -->
<!-- badges: end -->

ibfp is an R package that automates the formatting of sentences to create a variety of tasks in the Ibex software. The tasks include self-paced reading, L-maze, G-maze, comprehension questions, and acceptability judgments. There are several ways to host these tasks. The easiest way is to use [PC Ibex Farm](https://farm.pcibex.net/)

## Installation

You can install the ibfp package and load it into R as follows:

``` r
remotes::install_github("HirokiFujita1126/ibfp")
library(ibfp)

# You also need to load the dplyr package.
library(dplyr)
```

## Self-paced reading (SPR) / Comprehension questions

To create **a complete SPR task**, you need to prepare practice sentences, filler sentences and experimental sentences and format them in a text file as follows:

**practice sentence**  
p: It was clear that the little boy wanted a toy.  
pq: Did the little boy want a cake?  
po: Yes , No  

**experimental sentence**  
s: When the two actors met the director in the studio was talking about the film.  
s: When the two actors met, the director in the studio was talking about the film.  
sq: Did the two actors meet the director?  
sq: Did the two actors meet the director?  
so: Yes , No  
so: Yes , No  
sco: No  
sco: No  

**filler sentence**  
f: The radio that was near the window in the kitchen recently broke.  
fq: Did the radio break?  
fo: Yes , No  
fco: Yes  

p = practice sentence, s = experimental sentence, f = filler sentence, q = question, o = answer option, co = correct answer.

To run a phase by phase SPR, it is necessary to specify each phase by using an underscore as follows:  

s: Before the_woman dressed the_boy in_the_living_room was reading the_book.  

If there are more than two answer options, they can be formatted as follows: 

so: Yes , No, Not sure.  

When the ibfp package is loaded, a text file containing example sentences (Item.txt) is also loaded. Alternatively, you can load your own text file as follows:  

``` r
Material <- read.table("Item.txt", header = FALSE, col.names = paste0("c",seq_len(18)), fill = TRUE)
```

To create **a complete SPR task**, you first need to run the following R code:  

``` r
Setting <- SPR_Setting(
  NameOfPracticeItem = "P",
  NameOfFillerItem = "F",
  NameOfExperimentalItem = "E",
  RandomOrderForQuestion = FALSE) 
```
  
Here you need to give each type of sentence a unique identifier. In the example above, P, F and E are identifiers for practice, filler and experimental sentences respectively. The RandomOrderForQuestion argument specifies whether the order of the multiple-choice questions is randomised (e.g. sometimes 'Yes' or 'No', and sometimes 'No' or 'Yes') or not (e.g. always 'Yes' or 'No'). TRUE = randomised, FALSE = not randomised.

Next, run the following code to format the sentences.  

``` r
SPR(
  Setting = Setting,
  Material = Material,
  WordbyWordOrPhrasebyPhrase = TRUE,
  NameOfPracticeItem = "P",
  NumberOfCondition = 2,
  NameOfExperimentalItem = "E",
  NameOfFillerItem = "F")
```

Here, the Setting argument should be given the output of the SPR_Setting() function used earlier. The Material argument should be passed the data frame containing the practice, filler, and experimental sentences. The WordbyWordOrPhrasebyPhrase argument specifies whether each sentence should be presented word by word (or phrase by phrase) or as a whole. If set to TRUE, each sentence is presented word by word (or phrase by phrase if phrases are specified by underscores). If set to FALSE, 


The NameOfPracticeItem, NameOfExperimentalItem, and NameOfFillerItem should match those in the Setting argument. The NumberOfCondition argument must be set to the number of conditions. 

When the SPR() function is executed, a JavaScript file named “SPR.js” is generated in the working directory, which contains the sentences in Ibex format. One way to run this SPR task is to use PC Ibex Farm (https://farm.pcibex.net/). After creating and opening a project page, you can upload the JavaScript file to the Scripts box on the project page (you need to delete the original JavaScript file that is automatically created when you create a new project page). You can then run the SPR experiment both online and offline.

If you only want to create **a JavaScript file with only experimental or filler sentences**, you need to run the following code:

``` r
# For experimental sentences
SPRExp(
  Material = Material,
  NumberOfCondition = 2,
  NameOfExperimentalItem = "E",
  WordbyWordOrPhrasebyPhrase = TRUE,
  StartFrom = 1)
```

``` r
# For filler sentences
SPRFiller(Material = Material,
          NameOfFillerItem = "F",
          WordbyWordOrPhrasebyPhrase = TRUE,
          StartFrom = 17)
```

Here, you need to assign a number from which the trial number starts. For example, if you have 16 sets of experimental sentences and 50 filler sentences, you might want to assign 1 to SPRExp() and 17 to SPRFiller() so that the experimental sentences are numbered 1-16 and the filler sentences are numbered 17-66.

## Maze 

For maze tasks, there are several ways to automate the creation of distractors, e.g., [Boyce et al. (2020)](https://vboyce.github.io/Maze/) for G-maze and [Johnson (2022)](https://github.com/kennethgarlic/lmaze_automate) for L-maze, and these automate the formatting as well. In addition, you need to use the CSS and JavaScript files provided by [Boyce et al. (2020)](https://vboyce.github.io/Maze/) anyway, so this might really be for me (I often need to use this package for creating maze tasks for various reasons). To create **a complete maze task**, the sentences need to be formatted as follows:

p: The reporter had dinner yesterday with the baseball player who Kevin admired.  
pm: Thi resenter rad panner yestercip wich thi banctall sprager sno Kenon adsared.  

s: The lady near John dressed himself for the party.  
sm: Thi tagy nour Flnj dremmed himralf fom thi marby.  
s: The lady near John dressed herself for the party.  
sm: Thi tagy nour Flnj dremmed heralf fom thi marby.  

f: The small coin was picked up by the little boy in the garden last night.  
fm: Thi wrall coze wam pucked ip py thu lastle bok en thu ganpen lars niffs.  

Here, m is used to identify maze distractors. Then, you need to run the following code.

``` r
Setting <- Maze_Setting(
  NameOfPracticeItem = "P",
  NameOfFillerItem = "F",
  NameOfExperimentalItem = "E")

Maze(
  Setting = Setting,
  Material = Material,
  NameOfPracticeItem = "P",
  NumberOfCondition = 2,
  NameOfExperimentalItem = "E",
  NameOfFillerItem = "F")
```

To create **a JavaScript file with only experimental or filler sentences**, you need to run the following code:


``` r
MazeExp(
    Material = Material,
    NumberOfCondition = 2,
    NameOfExperimentalItem = "E",
    StartFrom = 1)

MazeFiller(
  Material = Material,
  NameOfFillerItem = "F",
  StartFrom = 17)
```

## Judgement

For judgement tasks, you can use the same format as for SPR / comprehension question tasks. To create **a complete judgement task**, you need to run the following code:

``` r
Setting <- Judgement_Setting(
  NameOfPracticeItem = "P",
  NameOfFillerItem = "F",
  NameOfExperimentalItem = "E",
  Scale = 1:7,
  MaxComment = "Highly acceptable",
  MinComment = "Highly unacceptable")

Judgement(
  Setting = Setting,
  Material = Material,
  NameOfPracticeItem = "P",
  NumberOfCondition = 2,
  NameOfExperimentalItem = "E",
  NameOfFillerItem = "F")
```

In the Setting function, you need to decide the scale and what the minimum and maximum values should indicate. To create **a JavaScript file with only experimental or filler sentences**, you need to run the following code:

``` r
JudgementExp(
  Material = Material,
  NumberOfCondition = 2,
  NameOfExperimentalItem = "E",
  StartFrom = 1)

JudgementFiller(
  Material = Material,
  NameOfFillerItem = "F",
  StartFrom = 17)
```

# Analysis of the 2016 Presidential Elections

This project tries to make sense of the shocking results of the 2016 presidential elections. First, using socio-economic and demographic variables, I project all American states on 2 dimensions and see how this projection explains the vote. The second code is a preliminary Machine Learning Algorithm predicting results in Swing States based on results in the rest of the country. Results are (for now) inconclusive.

Hope you enjoy! Feel free to get in touch with me at nicolas.jeanrenaud@gmail.com


## Understanding the 2016 Voting Structure

In this script, I perform a Principle Component Analysis on the US States based on 2016 socio-economic and demographic features. I then see how relevant the two first axes of the PCA are at explaining Donald Trump's election. The analysis is performed on all US States, and on a subset of 14 Swing States.

Results show that for both the entire United States and the Swing States, the clustering of US States using the first 2 axes of the PCA are relevant in explaining vote. Swing States in particular can be clustered into "Safe Democrat" (these states voted for Obama in 2012 and Clinton in 2016), "Safe Republican" (having voted for Romney in 2012 and Trump in 2016), and "The Swing of the Swing States": a subset of States that voted for Obama in 2012 and Trump in 2016.

A further analysis looks are the polling error in Donald Trump's favor. The polling numbers used here are the [final predictions of FiveThirtyEight.com](https://projects.fivethirtyeight.com/2016-election-forecast/). Preliminary calculations indicate that PCA axes 2 and 3 are actually the most relevant in explaining the polling error – indicating that unlike voting, the polling error was not due to the most important socio-economic and demographic fractures among Swing States.

The main findings are that Trump massively over-performed in older, whiter states, but also in Swing States with a strong African-American presence. [A post-mortem analysis in the Washington Post](https://www.washingtonpost.com/news/monkey-cage/wp/2017/05/08/why-did-trump-win-more-whites-and-fewer-blacks-than-normal-actually-voted/?noredirect=on&utm_term=.4ad3fbe49836) indicates that while Trump found his base in older, White voters, an sub-expectation vote among African-Americans also swung states like Michigan in his favor.

## Predicting Swing States Results based on the Rest of the USA

In this script, I develop a Machine Learning Model to predict the vote in Swing States based on the vote in the rest of the country. Results are inconclusive.

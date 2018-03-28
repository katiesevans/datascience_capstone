Data Science Coursera Capstone: Text Prediction
========================================================
author: Katie Evans
date: March 27, 2018
autosize: true

Introduction
========================================================
- This application aims to predict the next word from a user-defined text input
- This project is part of the Coursera Data Science Specialization Capstone Project, partnered with Swiftkey

![](coursera_logo.png)   ![](swiftkey_logo.png)

Predictive Model
========================================================
- The basics:
    - Input: word or phrase
    - Output: most likely next word
- Method:
    - Input cleaning
        - remove profanity, punctuation, and tokenize phrase
    - N-gram prediction
    
N-gram Prediction
========================================================
- An **n-gram** is a set of continuous words of length `n`
    - *Example*: 'go to', 'to the', and 'the movies' are all **bi-grams** for the phrase "go to the movies"
- The model acts by choosing the last 3 words of the input phrase and searching for the most popular quad-grams (4 word phrases).
    - If none are found, the last 2 words of the input phrase are used to search for the most popular tri-grams (3 word phrases)
    - This "back-off" method is continued until the most popular words are returned if the last word of the phrase is not matched with a bi-gram

Summary of Performance
========================================================
- This model is not the best predictive model
- Accuracy of predicting the last word in a set of test input phrases is about 25%
    - The actual last word is in the top 5 words predicted 25% of the time
- One of the biggest problems this model has is loss of context
    - If the beginning of the phrase is more important for the prediction than the end of the phrase, it will not be taken into account and thus the prediction will likely be wrong.

Shiny App
========================================================
- The shiny application can be found [here](https://katiesevans9.shinyapps.io/ngram_predictor/)
- The user types a word or phrase into the input text box on the left hand side, chooses the number of words to predict, and clicks the "Predict" button to start the application
- Momentarily, the application will print out the predicted words in order of most to least likely on the right-hand pane. 

***
![](shiny_app_pic.png)



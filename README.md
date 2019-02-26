# Ad Click Fraud Detection


This project is the second and final capstone project in the Harvard University Professional Certificate in Data Science program. I used the data and premise in the 2018 Kaggle competition hosted by TalkingData, which posed the question "How do we predict fraudulent ("bots") clicks" based on mobile data?"

TalkingData is China's largest independent big data service platform which covers over 70% of active mobile devices nationwide. They handle 3 billion clicks per day, of which 90% are potentially fraudulent. Their current approach to prevent click fraud for app developers is to measure the journey of a user’s click across their portfolio, and flag IP addresses who produce lots of clicks, but never end up installing apps. With this information, they've built an IP blacklist and device blacklist.

The goal of this project in the context of this capstone is to utilize multiple binary classification methods and algorithms to best predict fraudulent clicks. This project is relevant, because such clicks result in inflated costs by ad channels that claim to have high click rates. I used linear and radial kernal support vector machines as well as decision trees and random forests. I achieved an accuracy of 96% and F1 score of 98%. Other metrics explored include feature distributions, ROC curve (and AUC), and the various attributes of the confusion matrix (ie: specificity). 

# DataMIningFinalProject.github.io
https://williaddmw.github.io/DataMIningFinalProject.github.io/
Midway Report
Application of Machine Learning model in Diagnosis
Midway report
Group: Black Panthers

I.	Introduction

Complex medical diagnosis is frequently a lengthy and complicated process, occupying prolonged amounts of time for clients, physicians and healthcare staff. Subsequently, medical resources are often inefficiently allocated, and client costs may be excessive. Increasingly, healthcare organizations are finding solutions to lengthy referral and diagnosis issues with machine learning and electronic surveys that serve as a medical triage to classify symptoms and refer clients to appropriate diagnosis recourse. Faster accurate diagnosis results in improved client outcomes, as well as saving time and money for all involved, and allows for more clients to be seen and helped by healthcare professionals.
 BPPV (Benign Paroxysmal Positional Vertigo) is a common dizziness disease with    very high misdiagnosis rate. Consequently, clients and healthcare professionals alike stand to benefit from methods for faster more efficient diagnosis of BPPV. This study aims to improve BPPV diagnosis by implementing machine learning methods to procure an electronic survey based clinical support system consisting of a minimum set of questions that can achieve an accuracy of approximately 90% for BPPV diagnosis.
                  Relevant studies have implemented a linear pre-visit questionnaire to expedite BPPV   diagnosis (Friedland et. al),  and more recently a study by Richburg, Povinelli and Friedland developed an electronic survey questionnaire as a clinical support system for BPPV diagnosis with high accuracy and specificity using machine learning. [1] In addition, medical implementation of machine learning models to help diagnose illness is seen in studies implementing a self- referral decision support framework for low back pain as in “Evaluation of three machine learning models for self-referral decision support on low back pain in primary care”, and work by Ayeldeen et. al., “Prediction of Liver Fibrosis stages by Machine Learning model: A Decision Tree Approach”. [2][3] 
                  This project work is built on the framework of early two-phase survey conducted to diagnose  BBPV using decision tree [2]. Two iterations of survey and data analysis  have been conducted and our project is moving to the phase three and four of our  research. Our study will employ decision tree and ANN models to generate and classify diagnostic features of BPPV and predict outcomes using Weka based on survey results. Machine learning models such as random forest, Naive Bayes, K-Nearest Neighbors, Support Vector Machines in Python will compare decision trees and ANN models. Current accuracy rate using this model in diagnosis is around 70%. This study will implement the new models and examine accuracy for improvement, with considerations for the potential for certain noise in the data set. This project will continue to explore a novel ensemble approach to improve the accuracy of diagnosis for BPPV. Expectations are to develop a model with high accuracy, sensitivity and specificity in our prediction with the help of machine learning algorithms. Additional literary analysis will examine similar research to further understanding of potential methods to incorporate and deepen project understanding. 
II.	Related Work

Relevant studies include work by Friedland et. al., which implemented a linear pre-visit questionnaire to expedite BPPV diagnosis. The questions were administered prior to the office visit and concerned patient history in an effort to garner enough information to allow for a narrowed differential diagnosis before the first clinic visit. Because the questionnaire was overly lengthy-10 pages- it was prone to questionnaire discrepancies i.e., skipped questions, dishonest reporting, lack of question meaning, etc. [4]
More recently a study by Richburg, Povinelli and Friedland, developed an electronic survey questionnaire as a clinical support system for BPPV diagnosis with high accuracy and specificity using machine learning. [3] The electronic survey allowed for keeping relevant questions, and the ability to pass over questions that were not relevant. Study objective was to employ machine learning models to classify features as BPPV or not and thereby providing clinical support and accelerating diagnosis and treatment.[3]
In addition, medical implementation of machine learning models to help diagnose illness is seen in studies implementing a self- referral decision support framework for low back pain as in “Evaluation of three machine learning models for self-referral decision support on low back pain in primary care”, where, in an attempt to accurately prescribe the right intervention at the appropriate stage to circumvent condition attaining chronic status, specific machine learning models consisting of decision tree, random forest, and boosted tree techniques to classify low back pain cases.[2] Here, the boosted tree model performed best on the classification of low back pain cases, however,  the evaluation measures confirm that all models provided referral advice better than just a random guess, meaning that all models learned some implicit knowledge of the provided referral advices in the training dataset. Finally, research by Ayeldeen et. al., “Prediction of Liver Fibrosis stages by Machine Learning model: A Decision Tree Approach”, also used machine learning techniques to predict an individuals’ degree of liver fibrosis. Here, using decision tree classifier techniques, researchers were able to achieve accuracy rates of 93.7%, higher than studies with similar conditions were able to attain. [2][1] 


III. Datasets and Features
A.	Description of dataset
	The patient data is this study was collected from a survey questionnaire in Medical College of Wisconsin. The study coordinator in Medical College of Wisconsin would help the patients set up the tablets and assist them in filling out the survey in Android Tablets. The answers to the survey were stored in SQLite database when the survey was completed. The doctors in the hospital later would diagnose the patients if he or she has BPPV. Until the end of March in 2019, our research team has collected 74 patient surveys in phase one, 91 patient surveys in phase two, and 100 patient surveys in phase three. In total, there are 266 patient survey collected from three phases. The first phase of the survey contains 84 questions and began in May 2017. The second phase of the survey contains 68 questions and began in November 2017. [1] The third phase of the survey contains 42 questions and begin in August 2019 and is still going on. The survey contains six sections of questions with focus on symptoms of dizziness, timing of attacks, feeling of ear pain and headaches as well as triggers for dizziness and medical history. The phase two survey contains supplemental section. The supplemental section contains six questions and is used to test how well two decisions trees which are generated based on phase one survey would perform. 
B.	Combination of three survey questions
The project has implemented three versions of survey in three phases. Each version of survey has same overlapped questions with the other two. There are 84, 74, and 42 survey questions respectively in phase one, two, and three surveys. In order to efficiently use all of the patient data, we match the same questions from all three phases and get a combined data set. 
 

IV Methods
 Classification using machine learning methods. The following machine learning models will be implemented in this study:
1. Decision trees
The implementation of decision trees model consists of two steps. The first step is to obtain a subset of the most relevant questions using the correlation attribute evaluator in the Weka. The threshold of correlation attitude evaluator is 0.25. Then a J-48 decision tree algorithm in Weka will be used to classify each record as "Has BBPV" or " No BPPV". 
2. Ensemble of decision trees
The project also explores a novel ensemble approach to improve the accuracy of diagnosis for BPPV. This method will generate multiple trees using training data and make predication based on majority vote and weighted decision. Therefore, a group of trees instead of individual tress will be combined to predict BPPV disease.
3. ANN model 
ANN model is another machine learning framework that is inspired by biological neural network. The model takes features as input and results as output. There would be hidden layers called perceptron to process the information. We plan to use Weka to adjust the number of layers, the number of perceptron and learning rate to achieve the best prediction we could have.
4. KNN
KNN was a method of choice as the model structure is decided from the data- an outcome this study is interested in. This model makes predictions by calculating an input similarity to a training instance. [6] Not wanting to hold authentic medical data to assumptions, a goal of this research is to determine which survey questions provide the highest accuracy for BPPV diagnosis. KNN is often a go to classification method when prior knowledge about data distribution is scant. [6]
5. Naïve Bayes
Naive Bayes classification is based on Bayes theorem which provides us a way to calculate the probability of our classification based on existing data. 
[1]Bayes’ Theorem is stated as:
P(h|d) = (P(d|h) * P(h)) / P(d)
Where
•	P(h|d) is the probability of hypothesis h given the data d. This is called the posterior probability.
•	P(d|h) is the probability of data d given that the hypothesis h was true.
•	P(h) is the probability of hypothesis h being true (regardless of the data). This is called the prior probability of h.
•	P(d) is the probability of the data (regardless of the hypothesis).
You will notice that we are trying to calculate the conditional probability of P(h|d) from the prior probability p(h) with P(D) and P(d|h). Once we calculate the probability of a different number of hypothesis then we select the hypothesis with the highest probability.
Naive Bayes theorem is called Idiot Bayes because of the probability calculation of each hypothesis is made simpler. It is assumed in this theorem that features are independent of each other and do not have relation to each other what so ever.  
This is a very strong assumption and might not be true with most of the real-time data. However, this theorem seems to work well with most of the cases. [5]


 [1]H. Ayeldeen, O. Shaker , G. Ayeldeen, and K. M. Anwar, “Prediction of Liver Fibrosis stages by Machine Learning model: A Decision Tree Approach∗,” IEEE, Nov. 2015.
 [2]W. O. Nijeweme-d’Hollosya, L. van Velsen, M. Poel, C. G. M. Groothuis-Oudshoornd, R. Soer, and H. Hermens, “Evaluation of three machine learning models for self-referral decision support on low back pain in primary care,” International Journal of Medical Informatics, vol. 110, no. 31, Feb. 2018.
 [3]H. A. Richburg, R. J. Povinelli , and D. R. Friedland , “Direct-to-Patient Survey for Diagnosis of Benign Paroxysmal Positional Vertigo ,” 2018 17th IEEE International Conference on Machine Learning and Applications , Dec. 2018.
 [4]D. R. Friedland, S. Tarima, C. Erbe, and A. Miles, “Development of a Statistical Model for the Prediction of Common Vestibular Diagnoses,” JAMA Otolaryngology Head Neck Surgery., Apr. 2016.
[5] J. Brownlee, “Naive Bayes for Machine Learning,” Machine Learning Mastery, 11-Apr-2016. [Online]. Available: https://machinelearningmastery.com/naive-bayes-for-machine-learning/. [Accessed: 04-Apr-2019].
[6] A. Bronshtein, “KNN makes predictions just-in-time by calculating the similarity between an input sample and each training instance.,” Noteworthy- The Journal Blog, 11-Apr-2017. .

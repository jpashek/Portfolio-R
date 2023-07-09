# Portfolio-R
Portfolio work in Data Analytics in R

Project uses Dataset from Kaggle (https://www.kaggle.com/datasets/andreinovikov/used-cars-dataset).
Please use the link to view the dataset, as the dataset is too large to upload to GitHub.

Project focuses on using the dataset of used cars to predict prices of used vechicles, and give accurate prices and what factors are significant for the vechicles.
Since the dataset is so large, I focused on German automakers, however, the framework of the project allows for easy changes based on what indivduals are looking for or what used car dealerships are offering. 

Three regression models are used:

K-NN Regression: Nearest Neighbor regression model focuses on using closest neighbors to indivual example and gives us how many neigbors are optimal in using to find the lowest Mean Absolute Percentage Error.

Multiple Linear Regression: MLR regression focuses on finding which factors are significant to the overall price of the used vechicle, in the model we also use five example tibble vechicles to see what some vechicles prices could be, but also serves as a model so that the user can input the exact specifications of their vechicle and predict the price for sale or purchase.

Regression Tree: A decision tree that displays the hierarchy of important variables amongst the variables in a dataset. Used to show the power of R and their ability to give us visual aids in demonstrating the regression model.



Please review the code and feel free to comment about changes or errors found!
